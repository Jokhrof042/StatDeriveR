
#' Plot method for statderive objects
#'
#' @param x Object of class "statderive"
#' @param y Unused (required for S3 plot method)
#' @param ... Additional arguments
#'
#' @export
plot.statderive <- function(x, y = NULL, ...) {
  
  if (is.null(x$simulation)) {
    warning("No simulation data. Run simulate_check() first.")
    return(invisible(NULL))
  }
  
  if (x$type == "transform") {
    .plot_transform(x)
  } else if (x$type == "orderstat") {
    .plot_orderstat(x)
  }
  
  invisible(x)
}

#' Plot transformation with theory vs simulation
#' @keywords internal
.plot_transform <- function(x) {
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  y_vals <- x$simulation$simulated_values
  
  # Remove NA/Inf values for plotting
  y_vals <- y_vals[is.finite(y_vals)]
  
  if (length(y_vals) == 0) {
    warning("No finite values to plot")
    return(invisible(NULL))
  }
  
  # Create histogram
  hist(y_vals, 
       breaks = 50, 
       freq = FALSE, 
       main = sprintf("Transformation: Y = %s\nX ~ %s", 
                     x$transform, 
                     .format_dist(x$dist, x$params)),
       xlab = "Y", 
       ylab = "Density",
       col = rgb(0.5, 0.7, 0.95, 0.6), 
       border = "navy",
       cex.main = 1.2,
       cex.lab = 1.1)
  
  # Compute theoretical curve
  support <- x$new_support
  
  # Adjust limits if infinite
  if (is.infinite(support[1])) {
    support[1] <- min(y_vals) - abs(min(y_vals)) * 0.1
  }
  if (is.infinite(support[2])) {
    support[2] <- max(y_vals) + abs(max(y_vals)) * 0.1
  }
  
  # Generate sequence of y values
  y_seq <- seq(support[1], support[2], length.out = 500)
  
  # Evaluate pdf at each point
  pdf_vals <- tryCatch(
    sapply(y_seq, function(y) {
      tryCatch(x$pdf_function(y), error = function(e) NA)
    }),
    error = function(e) rep(NA, length(y_seq))
  )
  
  # Remove NA values from the curve
  valid_idx <- !is.na(pdf_vals)
  y_seq_valid <- y_seq[valid_idx]
  pdf_vals_valid <- pdf_vals[valid_idx]
  
  if (length(y_seq_valid) > 0) {
    # Add theoretical curve
    lines(y_seq_valid, pdf_vals_valid, 
          col = "darkred", 
          lwd = 2.5, 
          type = "l")
  }
  
  # Add legend
  legend("topright", 
         c("Simulated (n=5000)", "Theoretical PDF"),
         fill = c(rgb(0.5, 0.7, 0.95, 0.6), NA),
         border = c("navy", NA),
         lty = c(NA, 1),
         lwd = c(NA, 2.5),
         col = c(NA, "darkred"),
         cex = 1.0)
  
  # Add text box with statistics
  stats_text <- sprintf(
    "Mean: %.4f (Theory: %.4f)\nVar: %.4f (Theory: %.4f)",
    x$simulation$empirical_mean,
    x$simulation$theoretical_mean,
    x$simulation$empirical_var,
    x$simulation$theoretical_var
  )
  
  mtext(stats_text, 
        side = 1, 
        line = 4, 
        adj = 0, 
        cex = 0.9,
        family = "monospace")
}

#' Plot order statistic with theory vs simulation
#' @keywords internal
.plot_orderstat <- function(x) {
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  orderstat_vals <- x$simulation$simulated_values
  orderstat_vals <- orderstat_vals[is.finite(orderstat_vals)]
  
  if (length(orderstat_vals) == 0) {
    warning("No finite values to plot")
    return(invisible(NULL))
  }
  
  # Determine order label
  order_label <- switch(as.character(x$r),
    "1" = "minimum (X_(1))",
    paste0("X_(", x$r, ")")
  )
  if (x$r == x$n) order_label <- "maximum (X_(n))"
  
  # Create histogram
  hist(orderstat_vals, 
       breaks = 50, 
       freq = FALSE,
       main = sprintf("Order Statistic: %s (n=%d)\nX ~ %s", 
                     order_label, 
                     x$n,
                     .format_dist(x$dist, x$params)),
       xlab = "Value", 
       ylab = "Density",
       col = rgb(0.95, 0.7, 0.7, 0.6), 
       border = "darkred",
       cex.main = 1.2,
       cex.lab = 1.1)
  
  # Compute theoretical curve
  support <- x$support
  
  # Adjust limits if infinite
  if (is.infinite(support[1])) {
    support[1] <- min(orderstat_vals) - abs(min(orderstat_vals)) * 0.1
  }
  if (is.infinite(support[2])) {
    support[2] <- max(orderstat_vals) + abs(max(orderstat_vals)) * 0.1
  }
  
  # Generate sequence of values
  y_seq <- seq(support[1], support[2], length.out = 500)
  
  # Evaluate pdf at each point
  pdf_vals <- tryCatch(
    sapply(y_seq, function(y) {
      tryCatch(x$pdf_function(y), error = function(e) NA)
    }),
    error = function(e) rep(NA, length(y_seq))
  )
  
  # Remove NA values
  valid_idx <- !is.na(pdf_vals)
  y_seq_valid <- y_seq[valid_idx]
  pdf_vals_valid <- pdf_vals[valid_idx]
  
  if (length(y_seq_valid) > 0) {
    # Add theoretical curve
    lines(y_seq_valid, pdf_vals_valid, 
          col = "darkblue", 
          lwd = 2.5, 
          type = "l")
  }
  
  # Add legend
  legend("topright", 
         c("Simulated (n=5000)", "Theoretical PDF"),
         fill = c(rgb(0.95, 0.7, 0.7, 0.6), NA),
         border = c("darkred", NA),
         lty = c(NA, 1),
         lwd = c(NA, 2.5),
         col = c(NA, "darkblue"),
         cex = 1.0)
  
  # Add text box with statistics
  stats_text <- sprintf(
    "Mean: %.4f (Theory: %.4f)\nVar: %.4f (Theory: %.4f)",
    x$simulation$empirical_mean,
    x$simulation$theoretical_mean,
    x$simulation$empirical_var,
    x$simulation$theoretical_var
  )
  
  mtext(stats_text, 
        side = 1, 
        line = 4, 
        adj = 0, 
        cex = 0.9,
        family = "monospace")
}

