# tidyouwie
Package containing wrapper functions around OUwie to streamline analyses and export results in a neat tibble. 

# To Install
This package is not on CRAN. To install, run the following line of code:
```r
devtools::install_github("stfriedman/tidyouwie")
```

# Example R Code:
```r
# simulating data for example
phy <- pbtree(n = 20, nsim = 2) 
disc_trait <- setNames(sample(letters[1:2], 20, replace = TRUE), phy[[1]]$tip.label)
cont_traits <- as_tibble(iris[1:20, 1:2]) %>%
  mutate(species = phy[[1]]$tip.label)

# models to run
models <- c("BM1", "BMS", "OUM") 

#one line of code to run everything together
results <- ouwie_tidy(phy, disc_trait, cont_traits, models, nsim = 2)

results$tidy_output
```
