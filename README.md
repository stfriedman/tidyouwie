# tidyouwie
Package containing wrapper functions around OUwie to streamline analyses and export results in a neat tibble


# Example R Code:
#simulating dummy data to run through OUwie
phy <- pbtree(n = 20, nsim = 2)
disc_trait <- setNames(sample(letters[1:2], 20, replace = TRUE), phy[[1]]$tip.label)
cont_traits <- as_tibble(iris[1:20, 1:2]) %>%
  mutate(species = phy[[1]]$tip.label)

 models <- c("BM1", "BMS", "OUM") #set of models to run on each simmap

results <- ouwie_tidy(phy, disc_trait, cont_traits, models, nsim = 2)
results$tidy_output
