#' Wrapper OUwie function
#'
#' This function allows you to run OUwie across all combinations of trees, simmaps, models, and continuous traits and outputs analyses in a neat tibble.

#' @param phy Phylogenetic tree(s) containing all species/specimens present in dataset.
#' @param disc_trait Named vector of discrete characters where the names match the tree tips
#' @param cont_traits Data frame containing a character column with tree tips and any number of columns containing continuous trait data. Order of columns does not matter.
#' @param models Vector containing the set of models to run on each tree as designated in OUwie documentation
#' @param nsim Number of simmaps to create (for each tree) and run all analyses on
#' @param dir Optional; the directory to save output to, otherwise will save to working directory
#' @param tip_col Optional; column name in "cont_traits" that matches tree tips
#' @param params Optional; list of parameters to be passed to OUwie. NOTE: be aware that default OUwie parameters differ from the default OUwie documentation.
#' @return Function returns a list of the following
#' \item{input}{Simmaps and traits originally input into OUwie}
#' \item{full_output}{Raw tibble with all results}
#' \item{tidy_output}{Tidy tibble summarizing results}
#' @import OUwie
#' @import phytools
#' @import tidyverse
#'
#' @references Beaulieu, J. M., & O’Meara, B. 2014. OUwie: analysis of evolutionary rates in an OU framework.
#'
#' @examples

#' #simulating dummy data to run through OUwie
#' phy <- pbtree(n = 20, nsim = 2)
#' disc_trait <- setNames(sample(letters[1:2], 20, replace = TRUE), phy[[1]]$tip.label)
#' cont_traits <- as_tibble(iris[1:20, 1:2]) %>%
#'  mutate(species = phy[[1]]$tip.label)
#'
#' models <- c("BM1", "OUM") #set of models to run on each simmap
#'
#' results <- ouwie_tidy(phy, disc_trait, cont_traits, models, nsim = 2)
#' results$tidy_output
#' @export


ouwie_tidy <- function(phy, disc_trait, cont_traits, models, nsim, tip_col = NULL, dir = NULL, params = list(), ...) {
  inputs <- ouwie_setup(phy, disc_trait, cont_traits, nsim, tip_col, dir, ...)

  if(is.null(nsim)) nsim <- length(inputs$simtree)

  if(class(phy) == "multiPhylo"){
    nphy <- length(phy)
    nruns <- length(phy) * nsim * length(models) * length(unique(inputs$ouwie_input$trait))

    cat(paste("Running OUwie across", length(phy), "trees, with", nsim, "simmaps each,",
              length(models), "model(s), and", length(unique(inputs$ouwie_input$trait)),
              "continuous trait(s). This will be a total of", nruns,
              "runs. \n"))

    # creates unique combinations of each simmap, model, and continuous trait for each row of data, sets up the progress bar, and then separates dataset by simmap so intermediate results can be independently saved

    inputs$simtree <- structure(inputs$simtree,
                                class = c("list","multiSimmap","multiPhylo"))
    by_tree <- tibble(tree_id = rep(1:nphy, each = nsim),
                      simmap_id = rep(1:nsim, nphy)) %>%
      crossing(model = models) %>%  # make each combination of tree and model
      left_join(tibble(tree_id = rep(1:nphy, each = nsim), tree = inputs$simtree), by = "tree_id") %>%
      left_join(., inputs$ouwie_input, by = "tree_id") %>%
      #spawn_progbar() %>% #set up progress bar to run
      split(.$simmap_id)

  } else {

    if(nsim == 1) {
      inputs$simtree <- structure(inputs$simtree,
                                  class = c("list","simmap","phylo"))
      by_tree <- tibble(simmap_id = 1, tree = list(inputs$simtree))
      nruns <- length(models) * nrow(inputs$ouwie_input)

      #runs OUwie analysis on each combination of tree and model and trait,
      # only run once (will take a while depending on models run/num of simmaps)
      cat(paste("Running OUwie across 1 tree,", 1, "simmaps,",
                length(models), "model(s), and", nrow(inputs$ouwie_input),
                "continuous trait(s). This will be a total of", nruns,
                "runs. \n"))
    } else {
      inputs$simtree <- structure(inputs$simtree,
                                  class = c("list","multiSimmap","multiPhylo"))
      by_tree <- tibble(simmap_id = seq_along(inputs$simtree), tree = inputs$simtree)
      nruns <- length(inputs$simtree) * length(models) * nrow(inputs$ouwie_input)

      #runs OUwie analysis on each combination of tree and model and trait,
      # only run once (will take a while depending on models run/num of simmaps)
      cat(paste("Running OUwie across 1 tree,", length(inputs$simtree), "simmaps,",
                length(models), "model(s), and", nrow(inputs$ouwie_input),
                "continuous trait(s). This will be a total of", nruns,
                "runs. \n"))
    }
    by_tree <- by_tree %>%
      crossing(model = models) %>%  # make each combination of tree and model
      crossing(trait = inputs$ouwie_input$trait, data = inputs$ouwie_input$data,
               tree_id = inputs$ouwie_input$tree_id) %>%

      #spawn_progbar() %>% #set up progress bar to run
      split(.$simmap_id)
  }

  # resetting warn settings because otherwise bind_rows gets mad at using an unknown column class, simmap
  w <- options()$warn
  options(warn = -1)

  # run OUwie on each subset (simmap) of data and save intermediate output to designated director
  raw <- by_tree %>%
    map2(., seq_along(.), ~ouwie_wrapper(.x, dir, params))
  raw_output <- do.call(rbind, raw)

  options(warn = w)

  #compile OUwie results in a neat table
  ouwie_df <- raw_output %>%
    mutate(aicc = map_dbl(res, "AICc"),
           lnl = map_dbl(res, "loglik"),
           eigval = map(res, "eigval"),
           theta = map(res, ~.$theta[,1]),
           alpha = map(res, ~.$solution[1,]),
           sigma.sq = map(res, ~.$solution[2,])) %>%
    select(trait, everything(), -tree, -data, -res)

  out <- list(input = inputs, full_output = raw_output, tidy_output = ouwie_df)
  out
}

#data to be fed into OUwie, regimes are a named vector, traits are a data frame or tibble with species names and continuous trait data in columns, nsim is number of simmaps. Output are simmaps and a tibble of OUwie-ready data frames for each continuous trait
ouwie_setup <- function(phy, regimes, traits, nsim, tip_col, dir, ...) {
  simtree <- make.simmap(phy, regimes, nsim = nsim, ...) # make simmaps

  if(!is.null(tip_col)){
    nm <- tip_col
  } else {
    nm <- colnames(select_if(traits, function(col) is.factor(col) | is.character(col))) #get colname for tip names
    if(length(nm) != 1){
      stop("Can't determine column containing tip labels. Identify column name containing tip labels with the 'tip_col' parameter")
    }
  }

  if(class(phy) == "multiPhylo"){
    df_list = list()
    for(i in seq_len(length(phy))){
      disc_trait <- regimes[phy[[i]]$tip.label] # makes sure regimes are in same order as phylo

      df_list[[i]] <- as_tibble(traits) %>%
        rename_(tips = nm) %>%
        arrange(match(tips, phy[[i]]$tip.label)) %>% #match continuous trait data to phylo order
        mutate(disc_trait = regimes) %>%
        select(tips, disc_trait, everything()) %>%
        mutate(tree_id = i)
    }
    df <- bind_rows(df_list)

  } else {
    disc_trait <- regimes[phy$tip.label] # makes sure regimes are in same order as phylo

    df <- as_tibble(traits) %>%
      rename_(tips = nm) %>%
      arrange(match(tips, phy$tip.label)) %>% #match continuous trait data to phylo order
      mutate(disc_trait = regimes) %>%
      select(tips, disc_trait, everything()) %>%
      mutate(tree_id = 1)
  }

  # separates data into individual ouwie-ready data frames by row, with each row contianing a different trait. OUwie only takes dataframes with [1] species names, [2] discrete trait, and [3] single cont trait
  out <- df %>%
    gather(key="trait", value="trait_value", -tips, -disc_trait, -tree_id) %>%
    group_by(trait, tree_id) %>%
    nest() %>%
    mutate(data = map2(data, trait, function(x, name) {colnames(x)[3] <- name; x}))

  if(is.null(dir)){
    saveRDS(list(simtree, out), file = paste0("ouwie_inputs.RData"))
  } else {
    saveRDS(list(simtree, out), file = paste0(dir, "ouwie_inputs.RData"))
  }

  list(simtree = simtree, ouwie_input = out)
}


# # setting up progress bar for myOUwie function
# spawn_progbar <- function(x, .name = .pb, .times = 1) {
#   .name <- substitute(.name)
#   n <- nrow(x) * .times
#   eval(substitute(.name <<- dplyr::progress_estimated(n)))
#   x
# }


# OUwie function with attributes pre-set
new_ouwie <- function(tree, model, data, params){
  # printing progress bar
  #.pb$tick()$print()
  cat(paste0("Running trait: ", colnames(data)[[3]], "\n"))
  data <- as.data.frame(data)

  if(!is.null(params$root.age)) root.age = params$root.age else root.age = NULL
  if(!is.null(params$scaleHeight)) scaleHeight = params$scaleHeight else scaleHeight = FALSE
  if(!is.null(params$root.station)) root.station = params$root.station else root.station = FALSE
  if(!is.null(params$clade)) clade = params$clade else clade = NULL
  if(!is.null(params$mserr)) mserr = params$mserr else mserr = "none"
  if(!is.null(params$starting.vals)) starting.vals = params$starting.vals else starting.vals = NULL
  if(!is.null(params$check.identify)) check.identify = params$check.identify else check.identify = TRUE
  if(!is.null(params$algorithm)) algorithm = params$algorithm else algorithm = "invert"

  out <- OUwie(tree, data, model = model, simmap.tree = TRUE, quiet = TRUE,
               warn = FALSE, diagn = TRUE, root.age = root.age, scaleHeight = scaleHeight,
               root.station = root.station, clade = clade, mserr = mserr,
               starting.vals = starting.vals, check.identify = check.identify,
               algorithm = algorithm)
  out
}


# wraps up OUwie function by setting up progress bar for all analyses and saving output files for each tree independently
ouwie_wrapper <- function(x, dir, params) {

  # run ouwie on each simmap
  i <- x$simmap_id[[1]]

  # add in optional ouwie arguments
  if(length(params) == 0){
    out <- x %>% mutate(res = pmap(list(tree, model, data),
                                   ~new_ouwie(..1, ..2, ..3, params)))
  } else {
    out <- x %>% mutate(res = pmap(list(tree, model, data, params),
                                   ~new_ouwie(..1, ..2, ..3, ..4)))
  }

   # save intermediate output by simmap
  if(is.null(dir)){
    saveRDS(out, file = paste0("ouwie_output_tree_", i, ".RData"))
  } else {
    # saves files after each simmap is run
    saveRDS(out, file = paste0(dir, "ouwie_output_tree_", i, ".RData"))
  }

  return(out)
}

