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
numberset <- c(100,200,300,400,500)
result <- pcCrossValidation(X,Y,3,2,"euclidean",1,FALSE,numberset)
print(result)

## -----------------------------------------------------------------------------
plotCV(result)

