#' Generic Interface for Distance Metrics
#'
#' @title Distance Metric Interface
#' @description A generic interface for computing distances between observations
#' using either built-in or custom distance metrics.
#'
#' @param x Input data matrix or vector
#' @param method Either a character string specifying built-in method ("euclidean", "manhattan", "maximum") or a custom function
#' @param ... Additional arguments passed to methods
#' @return A distance object or matrix containing pairwise distances
#' @examples
#' # Using built-in method
#' x <- matrix(rnorm(100), ncol=2)
#' d1 <- distanceMetric(x, "euclidean")
#'
#' # Using custom method
#' custom_dist <- function(x) as.dist(crossprod(t(x)))
#' d2 <- distanceMetric(x, method=custom_dist)
#' @export
distanceMetric <- function(x, method = "euclidean", ...) {
  UseMethod("distanceMetric")
}

#' @describeIn distanceMetric Default method for built-in metrics
#' @export
distanceMetric.default <- function(x, method = "euclidean", ...) {
  # Check if method is a function (custom distance)
  if (is.function(method)) {
    result <- method(x, ...)
    if (!inherits(result, c("dist", "matrix"))) {
      stop("Custom distance function must return dist or matrix object", call. = FALSE)
    }
    return(result)
  }

  # Handle built-in string methods
  if (!is.character(method) || !method %in% c("euclidean", "manhattan", "maximum")) {
    stop("method must be one of: 'euclidean', 'manhattan', 'maximum', or a custom function", call. = FALSE)
  }
  stats::dist(x, method = method, ...)
}

#' Generic Interface for State Space Reconstruction
#'
#' @title State Space Reconstruction Interface
#' @description A generic interface for reconstructing state spaces from time series
#' data using either built-in or custom methods.
#'
#' @param x Input time series
#' @param E Embedding dimension (positive integer)
#' @param tau Time delay (positive integer)
#' @param method Optional custom function for state space reconstruction
#' @param ... Additional arguments passed to methods
#' @return A list containing the reconstructed state space components
#' @examples
#' \dontrun{
#' # Using default method
#' x <- rnorm(100)
#' s1 <- stateSpaceMethod(x, E=3, tau=2)
#'
#' # Using custom method
#' custom_space <- function(x, E, tau) {
#'   list(matrix=embed(x, E))
#' }
#' s2 <- stateSpaceMethod(x, E=3, tau=2, method=custom_space)
#' }
#' @export
stateSpaceMethod <- function(x, E, tau, method = NULL, ...) {
  # If method is provided, use custom reconstruction
  if (!is.null(method)) {
    if (!is.function(method)) {
      stop("method must be a function", call. = FALSE)
    }
    result <- method(x, E, tau, ...)
    if (!is.list(result) || !("matrix" %in% names(result))) {
      stop("Custom state space function must return a list with 'matrix' element",
           call. = FALSE)
    }
    return(result)
  }

  # Otherwise use method dispatch
  UseMethod("stateSpaceMethod")
}

#' @describeIn stateSpaceMethod Default state space reconstruction
#' @export
stateSpaceMethod.default <- function(x, E, tau, method = NULL, ...) {
  if (!is.numeric(E) || E < 2 || E != round(E)) {
    stop("E must be an integer greater than 1", call. = FALSE)
  }
  if (!is.numeric(tau) || tau < 1 || tau != round(tau)) {
    stop("tau must be a positive integer", call. = FALSE)
  }
  stateSpace(x, E, tau, ...)
}

