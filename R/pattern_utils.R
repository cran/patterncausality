#' Print Pattern Causality Pattern Analysis Results
#' Print Method for pc_pattern
#'
#' @param x A pc_pattern object
#' @param ... Additional arguments passed to print
#' @return Invisibly returns the input object
#' @keywords internal
#' @noRd
print.pc_pattern <- function(x, ...) {
  cat("Pattern Causality Pattern Analysis\n")
  cat("--------------------------------\n")
  cat("Number of patterns:", nrow(x$patterns), "\n")
  cat("Pattern dimension:", ncol(x$patterns), "\n")
  cat("Hash range:", range(x$hashes), "\n")
}

#' @keywords internal
#' @noRd
summary.pc_pattern <- function(object, ...) {
  structure(
    list(
      n_patterns = nrow(object$patterns),
      dimension = ncol(object$patterns),
      hash_range = range(object$hashes),
      pattern_stats = summary(as.vector(object$patterns)),
      hash_stats = summary(object$hashes)
    ),
    class = "summary.pc_pattern"
  )
}

#' Print Method for pc_pattern Summary
#'
#' @param x A summary.pc_pattern object
#' @param ... Additional arguments passed to print
#' @return Invisibly returns the input object
#' @keywords internal
#' @noRd
print.summary.pc_pattern <- function(x, ...) {
  cat("Pattern Analysis Summary\n")
  cat("----------------------\n")

  if (!is.null(x$total_patterns)) {
    cat("Total patterns:", x$total_patterns, "\n")
  }

  if (!is.null(x$unique_patterns)) {
    cat("Unique patterns:", x$unique_patterns, "\n")
  }

  if (!is.null(x$hash_stats)) {
    cat("Hash Statistics:\n")
    print(x$hash_stats, ...)
  }

  invisible(x)
}