## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(patterncausality)
data(DJS)
#head(DJS)

## ----eval=FALSE---------------------------------------------------------------
#  dataset <- DJS[,-1] # remove the date column
#  result <- pcMatrix(dataset, E = 3, tau = 1, metric = "euclidean", h = 2, weighted = TRUE)

## ----echo=FALSE---------------------------------------------------------------
result <- readRDS("djsmatrix.rds")

## -----------------------------------------------------------------------------
head(result$positive)

## -----------------------------------------------------------------------------
plotMatrix(result, status = "positive", method = "circle")

## -----------------------------------------------------------------------------
plotMatrix(result, status = "negative", method = "circle")

## -----------------------------------------------------------------------------
plotMatrix(result, status = "dark", method = "circle")

## -----------------------------------------------------------------------------
effects <- pcEffect(result)
print(effects)

## -----------------------------------------------------------------------------
plotEffect(effects, "negative",TRUE)

