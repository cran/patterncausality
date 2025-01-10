## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  warning = FALSE,
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
    fig.height = 6
)

## ----message=FALSE------------------------------------------------------------
library(patterncausality)
data(DJS)
#head(DJS)

## ----eval=FALSE---------------------------------------------------------------
# dataset <- DJS[,-1] # remove the date column
# params <- optimalParametersSearch(
#   Emax = 3,
#   tauMax = 3,
#   metric = "euclidean",
#   dataset = dataset,
#   verbose = FALSE
# )
# print(params)

## ----eval=FALSE---------------------------------------------------------------
# result <- pcMatrix(
#   dataset = dataset,
#   E = 3,           # Embedding dimension
#   tau = 1,         # Time delay
#   metric = "euclidean",
#   h = 1,           # Prediction horizon
#   weighted = FALSE  # Unweighted analysis
# )

## ----echo=FALSE---------------------------------------------------------------
result <- readRDS("DJSm.rds")
result$is_square <- TRUE

## -----------------------------------------------------------------------------
print(result)

## -----------------------------------------------------------------------------
plot(result, "positive")

## -----------------------------------------------------------------------------
plot(result, "negative")

## -----------------------------------------------------------------------------
plot(result, "dark")

## -----------------------------------------------------------------------------
effects <- pcEffect(result)
print(effects)

## -----------------------------------------------------------------------------
plot(effects, status="positive")

## -----------------------------------------------------------------------------
plot(effects, status="negative")

## -----------------------------------------------------------------------------
plot(effects, status="dark")

## -----------------------------------------------------------------------------
dataset <- DJS[, -1]

X <- dataset[, 1:10]
Y <- dataset[, 11:29]

## ----echo=FALSE---------------------------------------------------------------
result_cross <- readRDS("djscross.rds")

## ----eval=FALSE---------------------------------------------------------------
# result_cross <- pcCrossMatrix(
#   X = X,
#   Y = Y,
#   E = 3,
#   tau = 1,
#   metric = "euclidean",
#   h = 1,
#   weighted = FALSE,
#   verbose = FALSE
# )

## -----------------------------------------------------------------------------
plot(result_cross, "positive")

