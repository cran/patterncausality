## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(patterncausality)
data(climate_indices)

## -----------------------------------------------------------------------------
X <- climate_indices$AO
Y <- climate_indices$AAO
result <- pcLightweight(X, Y, E = 3, tau = 1, metric = "euclidean", h = 1, weighted = TRUE, verbose=FALSE)

## -----------------------------------------------------------------------------
result <- pcFullDetails(X, Y, E = 3, tau = 1, metric = "euclidean", h = 1, weighted = TRUE, verbose=FALSE)
print(result)

## -----------------------------------------------------------------------------
plot_causality(result, type="total")

## -----------------------------------------------------------------------------
plot_causality(result, type="positive")

## -----------------------------------------------------------------------------
result <- pcFullDetails(X, Y, E = 3, tau = 1, metric = "euclidean", h = 1, weighted = FALSE, verbose=FALSE)
print(result)

## -----------------------------------------------------------------------------
plot_causality(result, type="total")

