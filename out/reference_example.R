#' Reproduces reference example
#' @returns summary of sample size calculation

library(gsDesign)
# reproducing the sample size calculation (exponential model) I obtain a larger N (+200 patients ca)
h_c <- 0.05/12
HR <- 0.775
res <- nSurvival(h_c, h_c*HR, 30, 24, alpha = 0.05, sided = 2)
res
res$n
res$nEvents
