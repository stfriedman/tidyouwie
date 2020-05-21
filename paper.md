---
title: 'Tidyouwie: A tidy approach for streamlining ‘OUwie’ analyses'
tags:
  - macroevolution
  - trait evolution
  - model fitting
  - Ornstein-Uhlenbeck
  - Brownian motion

authors:
  - name: Sarah T. Friedman
    orcid: 0000-0003-0192-5008
    affiliation: 1

affiliations:
 - name: Department of Ecology and Evolution, University of California, Davis
   index: 1
date: 12 Aug 2019
bibliography: paper.bib
---

# Summary
Phylogenetic comparative methods are a suite of analytic tools that are used to study species in an evolutionary framework, taking into account the non-independence of lineages. Many of the questions that motivate comparative biologists involve comparing the values of continuous traits and how they vary across species on the phylogeny [@Butler:2004]. To answer such questions, a form of hypothesis testing using explicit models of trait evolution is commonly implemented. By evaluating the fit of these models to the data, the dynamics of character evolution can be inferred based on the parameters of the best fitting model.

``OUwie`` [@Beaulieu:2015] is a widely-used R package used to implement this model-fitting framework with two primary models: Brownian motion (BM) and Ornstein-Uhlenbeck (OU) models [@Hansen:1997;@Beaulieu:2015]. Under a BM model, the amount of phenotypic change is expected to be proportional to time [@Felsenstein:1985]. In contrast, OU models describe trait evolution as an adaptive process, accommodating variability in three parameters—the primary optima (theta), the strength of stabilizing selection (alpha), and the rate of evolution (sigma). Different OU models used in the model fitting framework will selectively allow the parameters to vary with regime, with the most complex OU model allowing all three of these parameters to vary with each regime. 

``Tidyouwie`` is an R package designed to streamline the model-fitting framework by creating a tidyverse-compatible wrapper for the R package ``OUwie`` [@Beaulieu:2015]. Currently, users have to create their own methods of iteration for each phylogeny, trait, and model they want to run. There is no framework for which to do so in the current ``OUwie`` package, making evolutionary model-fitting potentially prone to errors and bugs. ``Tidyouwie`` serves to streamline the entire process, taking any number of phylogenies, stochastic character maps (see [@Bollback:2006]), models, and continuous traits, internally making all possible combinations of all parameters and running the model-fitting process with a single line of code. Outputting the results in a neat tibble makes post-analysis model comparison very easy and straightforward. Additional functions included in the package allow users to plot all parameters for each trait to visually compare model fits (``plot_params``) and expand the parameters within the tibble to compare values across regimes (``expand_params``). It should be acknowledged that this package heavily depends on code published in the ``OUwie`` package [@Beaulieu:2015], as well as the tidyverse set of packages. 


# Acknowledgements
This work was made possible by funding through the UC Davis Graduate Group in Ecology.

# References