## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----message = FALSE----------------------------------------------------------
library(patterncausality)
data(climate_indices)

## -----------------------------------------------------------------------------
set.seed(123)
X <- climate_indices$PNA
Y <- climate_indices$NAO
result <- pcCrossValidation(
  X = X,
  Y = Y,
  numberset = seq(100, 500, by = 10),
  E = 3,
  tau = 2,
  metric = "euclidean",
  h = 1,
  weighted = FALSE
)
print(result$results)

## -----------------------------------------------------------------------------
plot(result)

## -----------------------------------------------------------------------------
set.seed(123)
X <- climate_indices$PNA
Y <- climate_indices$NAO
result_non_random <- pcCrossValidation(
  X = X,
  Y = Y,
  numberset = seq(100, 500, by = 100),
  E = 3,
  tau = 2,
  metric = "euclidean",
  h = 1,
  weighted = FALSE,
  random = FALSE
)
print(result_non_random$results)

## -----------------------------------------------------------------------------
plot(result_non_random)

## -----------------------------------------------------------------------------
set.seed(123)
X <- climate_indices$PNA
Y <- climate_indices$NAO
result_boot <- pcCrossValidation(
  X = X,
  Y = Y,
  numberset = seq(100, 500, by = 100),
  E = 3,
  tau = 2,
  metric = "euclidean",
  h = 1,
  weighted = FALSE,
  random = TRUE,
  bootstrap = 10  # Perform 100 bootstrap iterations
)

## -----------------------------------------------------------------------------
print(result_boot$results)

## -----------------------------------------------------------------------------
plot(result_boot, separate = TRUE)

