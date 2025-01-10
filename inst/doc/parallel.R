## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 6
)

## ----message = FALSE----------------------------------------------------------
library(patterncausality)
data(climate_indices)

## -----------------------------------------------------------------------------
X <- climate_indices$PNA
Y <- climate_indices$NAO

## -----------------------------------------------------------------------------
run_cv_test <- function(n_cores) {
    start_time <- Sys.time()
    result <- pcCrossValidation(
        X = X,
        Y = Y,
        numberset = c(100, 200, 300, 400, 500),
        E = 3,
        tau = 2,
        metric = "euclidean",
        h = 1,
        weighted = FALSE,
        random = TRUE,
        bootstrap = 100,
        n_cores = n_cores,
        verbose = TRUE
    )
    end_time <- Sys.time()
    return(difftime(end_time, start_time, units = "secs"))
}

