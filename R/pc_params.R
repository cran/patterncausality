#' Pattern Causality Parameter Optimization Results
#'
#' @title Pattern Causality Parameter Optimization Results
#' @description Creates an object containing parameter optimization results for pattern causality analysis
#' @param accuracy_summary Data frame containing accuracy results for different parameter combinations
#' @param computation_time Time taken for optimization
#' @param parameters List of optimization parameters
#' @return An object of class "pc_params"
#' @export
pc_params <- function(accuracy_summary, computation_time, parameters) {
  # Find the best parameter combination (highest Total)
  max_total_idx <- which.max(accuracy_summary$Total)
  best_params <- list(
    E = accuracy_summary$E[max_total_idx],
    tau = accuracy_summary$tau[max_total_idx]
  )
  
  structure(
    list(
      accuracy_summary = accuracy_summary,
      computation_time = computation_time,
      parameters = parameters,
      best_params = best_params
    ),
    class = "pc_params"
  )
}

#' Print Method for Pattern Causality Parameter Results
#'
#' @param x A pc_params object
#' @param verbose Logical; whether to display detailed information
#' @param ... Additional arguments passed to print
#' @return Invisibly returns the input object
#' @export
#' @method print pc_params
print.pc_params <- function(x, verbose = FALSE, ...) {
  cat("Pattern Causality Parameter Optimization Results\n")
  cat("---------------------------------------------\n")

  cat("Parameters tested:\n")
  cat("  Emax:", x$parameters$Emax, "\n")
  cat("  tauMax:", x$parameters$tauMax, "\n")
  cat("  Metric:", x$parameters$metric, "\n\n")
  
  # Create a copy of the results without combo row names
  results_df <- x$accuracy_summary
  rownames(results_df) <- NULL
  
  cat("All parameter combinations:\n")
  print(results_df, row.names = FALSE, ...)
  
  cat("\n")
  invisible(x)
}

#' Summary Method for Pattern Causality Parameter Results
#'
#' @param object A pc_params object
#' @param ... Additional arguments passed to summary
#' @return A summary object for pc_params
#' @export
#' @method summary pc_params
summary.pc_params <- function(object, ...) {
  # Calculate summary statistics for each metric
  accuracy_stats <- list(
    Total = summary(object$accuracy_summary$Total),
    Positive = summary(object$accuracy_summary$Positive),
    Negative = summary(object$accuracy_summary$Negative),
    Dark = summary(object$accuracy_summary$Dark)
  )
  
  structure(
    list(
      computation_time = object$computation_time,
      parameters = object$parameters,
      best_params = object$best_params,
      accuracy_stats = accuracy_stats,
      n_combinations = nrow(object$accuracy_summary)
    ),
    class = "summary.pc_params"
  )
}

#' Print method for summary(pc_params)
#'
#' @param x An object of class "summary.pc_params"
#' @param ... Passed to underlying print methods
#' @exportS3Method print summary.pc_params
print.summary.pc_params <- function(x, ...) {
  cat("Pattern Causality Parameter Optimization Summary\n")
  cat("----------------------------------------------\n")
  
  if (!is.null(x$n_combinations)) {
    cat("Total combinations tested:", x$n_combinations, "\n\n")
  }
  
  if (!is.null(x$parameters)) {
    cat("Search parameters:\n")
    cat("  Emax:", x$parameters$Emax, "\n")
    cat("  tauMax:", x$parameters$tauMax, "\n") 
    cat("  Metric:", x$parameters$metric, "\n\n")
  }
  
  if (!is.null(x$best_params)) {
    cat("Best parameter combination (highest Total):\n")
    cat("  E =", x$best_params$E, ", tau =", x$best_params$tau, "\n\n")
  }
  
  if (!is.null(x$accuracy_stats)) {
    cat("Summary statistics across all combinations:\n")
    for (metric in names(x$accuracy_stats)) {
      cat("\n", metric, ":\n", sep = "")
      stats <- x$accuracy_stats[[metric]]
      cat("  Min:", sprintf("%.6f", stats[1]), "\n")
      cat("  Mean:", sprintf("%.6f", stats[4]), "\n") 
      cat("  Max:", sprintf("%.6f", stats[6]), "\n")
    }
  }
  
  invisible(x)
}

# Helper function for creating accuracy summary
create_accuracy_summary <- function(results, E_array, tau_array) {
  accuracy_df <- data.frame(
    E = rep(E_array, each = length(tau_array)),
    tau = rep(tau_array, times = length(E_array)),
    Total = as.vector(t(results$total)),
    Positive = as.vector(t(results$positive)),
    Negative = as.vector(t(results$negative)),
    Dark = as.vector(t(results$dark))
  )
  accuracy_df <- accuracy_df[order(accuracy_df$E, accuracy_df$tau),]
  rownames(accuracy_df) <- sprintf("Combo %d", seq_len(nrow(accuracy_df)))

  return(accuracy_df)
}