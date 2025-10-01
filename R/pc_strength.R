pc_strength <- function(real, predicted) {
  structure(
    list(
      real = real,
      predicted = predicted
    ),
    class = "pc_strength"
  )
}

#' Print Method for pc_strength
#'
#' @param x A pc_strength object
#' @param ... Additional arguments passed to print
#' @return Invisibly returns the input object
#' @keywords internal
#' @noRd
print.pc_strength <- function(x, ...) {
  cat("Pattern Causality Strength Analysis\n")
  cat("---------------------------------\n")
  cat("Real strength:", x$real, "\n")
  cat("Predicted strength:", x$predicted, "\n")
  invisible(x)
}