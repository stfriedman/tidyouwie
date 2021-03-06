#' Expand parameters from OUwie output
#'
#' This function expands the output from ouwie_tidy so that parameter values for all regimes are visible.

#' @param results output from the ouwie_tidy function

#' @return Function returns the results tibble with parameter values for each regime in a column
#' @references Beaulieu, J. M., & O’Meara, B. 2014. OUwie: analysis of evolutionary rates in an OU framework.
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
#' expand_params(results)

#' @export
# reads in an ouwie_tidy object and prints parameters of each regime in columns
expand_params <- function(results) {
  regimes <- colnames(results$input$simtree[[1]]$mapped.edge)

  results$tidy_output %>%
    gather(theta:sigma.sq, key = "param", value = "value") %>%
    mutate(value = map(value, ~set_names(.x, regimes[seq_along(.)])),
           value = map(value, ~enframe(.x, name = "regime"))) %>%
    unnest(value) %>%
    unite("name", param, regime) %>%
    filter(name != "theta_NA") %>%
    pivot_wider(names_from = "name", values_from = "value",
      values_fn = list(value = list)) %>%
    unnest(-eigval)
  }
