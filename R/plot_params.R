#' Plot parameters from OUwie output
#'
#' This function plots the output from ouwie_tidy so that all regimes are visible. Separate plots created for each continuous trait

#' @param results output from the ouwie_tidy function
#' @return Function returns a plot summarizing the model comparisons for each continuous trait in the results output
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
#' results <- ouwie_tidy(phy, disc_trait, cont_traits, models, nsim = 2)#'
#' plot_params(results)
#' @export

# plots alpha, sigma, and theta parameters for each model and regime; separate plots for each continuous trait in dataset
plot_params <- function(results) {
  regimes <- levels(results$full_output$res[[1]]$tot.states)

  p <- results$tidy_output %>%
    gather(theta:sigma.sq, key = "param", value = "value") %>%
    mutate(value = map(value, ~set_names(.x, regimes[seq_along(.)])),
           value = map(value, ~enframe(.x, name = "regime"))) %>%
    unnest(value, .drop = FALSE)

  traits <- unique(results$tidy_output$trait)
  nmods <- unique(results$tidy_output$model)

  for(i in seq_along(traits)) {
    p1 <- p %>%
      mutate(tree_id = as.character(tree_id)) %>%
      filter(trait == traits[[i]])

    # have to use weird plotting method to get axes labelled right and to scale plots individually, made a combined graph using facet_wrap and facet_grid
    #plot that has title aesthetics
    p1$param[p1$param == "sigma.sq"] <- "sigma"
    ggo <- ggplot(p1, aes(fill = regime)) +
      ggtitle(traits[[i]]) +
      geom_density(aes(value)) +
      facet_grid(param ~ model, switch = "y", labeller = label_parsed) +
      theme_classic() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(angle = 180, size=15, face = "bold"),
        axis.title =  element_blank()
      )

    # plot that has graph aesthetics
    ggp <- ggplot(p1, aes(fill = regime)) +
      geom_density(aes(value), alpha = 0.6, col = NA) +
      facet_wrap(param ~ model, scales="free", nrow = 3, ncol = nmods,
                 labeller = labeller(multi_line = FALSE)) +
      theme_classic() +

      #adding y and x axis lines to each plot
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      theme(
        panel.spacing.x = unit(1, "lines"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line = element_line(size = 0.1),
        axis.ticks.x = element_line(size = 0.1),
        axis.ticks.y = element_line(size = 0.1),
        axis.text = element_text(size = 7),
        plot.margin = unit(c(2,0,1,2), "lines")
      ) +
      scale_fill_viridis(discrete = TRUE)

    #only picking specific elements of each plot and combining
    gt <- ggplotGrob(ggp)
    gto <- ggplotGrob(ggo)
    grid_titles <- gtable_filter(gto,"(title|strip-t|strip-l)", trim = FALSE)
    wrap_graphs <- gtable_filter(gt,"(panel|axis-b|axis-l|guide-box)", trim = FALSE)
    grid.newpage()
    grid.draw(grid_titles)
    grid.draw(wrap_graphs)
  }
}
