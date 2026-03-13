
#' Print method for statderive objects
#'
#' @param x Object of class "statderive"
#' @param ... Additional arguments
#'
#' @export
print.statderive <- function(x, ...) {
  
  cat("\n", strrep("=", 70), "\n")
  cat("StatDeriveR: Mathematical Statistics Derivation\n")
  cat(strrep("=", 70), "\n\n")
  
  if (x$type == "transform") {
    .print_transform(x)
  } else if (x$type == "orderstat") {
    .print_orderstat(x)
  }
  
  if (!is.null(x$simulation)) {
    .print_simulation(x)
  }
  
  cat("\n", strrep("=", 70), "\n\n")
  invisible(x)
}

#' Print transformation derivation
#' @keywords internal
.print_transform <- function(x) {
  
  cat("PROBLEM:\n")
  cat(sprintf("  Find the distribution of Y = %s\n", x$transform))
  cat(sprintf("  where X ~ %s\n\n", .format_dist(x$dist, x$params)))
  
  cat("ASSUMPTIONS:\n")
  for (i in seq_along(x$assumptions)) {
    cat(sprintf("  • %s\n", x$assumptions[i]))
  }
  cat("\n")
  
  cat("DERIVATION STEPS:\n")
  for (i in seq_along(x$steps)) {
    cat(sprintf("  %d. %s\n", i, x$steps[i]))
  }
  cat("\n")
  
  cat("SUPPORT:\n")
  cat(sprintf("  Original X: (%.6g, %.6g)\n", x$original_support[1], x$original_support[2]))
  cat(sprintf("  Transformed Y: (%.6g, %.6g)\n\n", x$new_support[1], x$new_support[2]))
  
  cat("KEY COMPONENTS:\n")
  cat(sprintf("  Inverse transformation: %s\n", x$inverse))
  cat(sprintf("  Derivative: %s\n\n", x$derivative))
  
  cat("FINAL RESULT:\n")
  cat(sprintf("  %s\n\n", x$pdf_formula))
}

#' Print order statistic derivation
#' @keywords internal
.print_orderstat <- function(x) {
  
  order_label <- switch(as.character(x$r),
    "1" = "minimum (X_(1))",
    paste0("X_(", x$r, ")")
  )
  if (x$r == x$n) order_label <- "maximum (X_(n))"
  
  cat("PROBLEM:\n")
  cat(sprintf("  Find the distribution of %s\n", order_label))
  cat(sprintf("  from %d i.i.d. samples of X ~ %s\n\n", x$n, .format_dist(x$dist, x$params)))
  
  cat("ASSUMPTIONS:\n")
  for (i in seq_along(x$assumptions)) {
    cat(sprintf("  • %s\n", x$assumptions[i]))
  }
  cat("\n")
  
  cat("DERIVATION STEPS:\n")
  for (i in seq_along(x$steps)) {
    cat(sprintf("  %d. %s\n", i, x$steps[i]))
  }
  cat("\n")
  
  cat("SUPPORT:\n")
  cat(sprintf("  (%.6g, %.6g)\n\n", x$support[1], x$support[2]))
  
  cat("FINAL RESULT:\n")
  cat(sprintf("  %s\n\n", x$formula))
}

#' Print simulation results
#' @keywords internal
.print_simulation <- function(x) {
  
  cat("SIMULATION VERIFICATION (", x$simulation$n_sim, " replications):\n", sep = "")
  cat("\n")
  
  # Print the comparison table with nice formatting
  comp_df <- x$simulation$comparison
  
  # Create formatted output
  cat(sprintf("%-12s %15s %15s %15s\n", "Statistic", "Empirical", "Theoretical", "Error"))
  cat(strrep("-", 60), "\n")
  
  for (i in 1:nrow(comp_df)) {
    cat(sprintf("%-12s %15.6f %15.6f %15.6f\n",
      comp_df$Statistic[i],
      comp_df$Empirical[i],
      comp_df$Theoretical[i],
      comp_df$Error[i]
    ))
  }
  
  cat("\n")
  cat("SUMMARY:\n")
  cat(sprintf("  Simulated %d valid observations\n", x$simulation$n_finite))
  cat(sprintf("  Mean absolute error: %.6f\n", x$simulation$mean_error))
  cat(sprintf("  Variance absolute error: %.6f\n", x$simulation$var_error))
  cat("\n")
}

