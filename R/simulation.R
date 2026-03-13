
#' Run Monte Carlo simulation and compare with theory
#'
#' @param object Object of class "statderive" from derive_transform or derive_orderstat
#' @param n_sim Integer: number of simulations (default 10000)
#' @param seed Integer: random seed for reproducibility
#'
#' @return Updated object with simulation results added
#' @export
#'
#' @examples
#' obj <- derive_transform("uniform", list(min = 0, max = 1), "x^2")
#' obj_sim <- simulate_check(obj, n_sim = 5000)
simulate_check <- function(object, n_sim = 10000, seed = 123) {
  
  if (!inherits(object, "statderive")) {
    stop("object must be of class statderive")
  }
  
  set.seed(seed)
  
  if (object$type == "transform") {
    sim_result <- .simulate_transform(object, n_sim)
  } else if (object$type == "orderstat") {
    sim_result <- .simulate_orderstat(object, n_sim)
  } else {
    stop("Unknown statderive type")
  }
  
  object$simulation <- sim_result
  return(object)
}

#' Simulate transformed variable
#' @keywords internal
.simulate_transform <- function(object, n_sim) {
  
  rng <- .rng_eval(object$dist, object$params, n_sim)
  
  # Apply transformation
  Y <- switch(object$transform,
    "x^2" = rng^2,
    "sqrt(x)" = sqrt(rng),
    "1/x" = 1 / rng,
    "log(x)" = log(rng),
    "exp(x)" = exp(rng)
  )
  
  # Remove infinite values for moment calculation
  Y_finite <- Y[is.finite(Y)]
  
  # Compute empirical moments
  empirical_mean <- mean(Y_finite, na.rm = TRUE)
  empirical_var <- var(Y_finite, na.rm = TRUE)
  empirical_sd <- sd(Y_finite, na.rm = TRUE)
  
  # Theoretical moments (numerical integration)
  support <- object$new_support
  
  # Handle infinite support limits for integration
  lower_limit <- support[1]
  upper_limit <- support[2]
  
  if (is.infinite(lower_limit)) {
    lower_limit <- min(Y_finite) - abs(min(Y_finite)) * 0.5
  }
  if (is.infinite(upper_limit)) {
    upper_limit <- max(Y_finite) + abs(max(Y_finite)) * 0.5
  }
  
  theoretical_mean <- tryCatch(
    integrate(function(y) y * object$pdf_function(y), 
              lower_limit, upper_limit, subdivisions = 100)$value,
    error = function(e) NA
  )
  
  theoretical_var <- tryCatch(
    integrate(function(y) (y^2) * object$pdf_function(y),
              lower_limit, upper_limit, subdivisions = 100)$value - theoretical_mean^2,
    error = function(e) NA
  )
  
  list(
    simulated_values = Y,
    n_sim = n_sim,
    n_finite = length(Y_finite),
    empirical_mean = empirical_mean,
    empirical_var = empirical_var,
    empirical_sd = empirical_sd,
    theoretical_mean = theoretical_mean,
    theoretical_var = theoretical_var,
    theoretical_sd = sqrt(abs(theoretical_var)),
    mean_error = abs(empirical_mean - theoretical_mean),
    var_error = abs(empirical_var - theoretical_var),
    comparison = data.frame(
      Statistic = c("Mean", "Variance", "Std Dev"),
      Empirical = c(empirical_mean, empirical_var, empirical_sd),
      Theoretical = c(theoretical_mean, theoretical_var, sqrt(abs(theoretical_var))),
      Error = c(
        abs(empirical_mean - theoretical_mean),
        abs(empirical_var - theoretical_var),
        abs(empirical_sd - sqrt(abs(theoretical_var)))
      )
    )
  )
}

#' Simulate order statistic
#' @keywords internal
.simulate_orderstat <- function(object, n_sim) {
  
  orderstat_samples <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    sample <- .rng_eval(object$dist, object$params, object$n)
    sorted <- sort(sample)
    orderstat_samples[i] <- sorted[object$r]
  }
  
  # Remove infinite values
  orderstat_finite <- orderstat_samples[is.finite(orderstat_samples)]
  
  # Empirical moments
  empirical_mean <- mean(orderstat_finite)
  empirical_var <- var(orderstat_finite)
  empirical_sd <- sd(orderstat_finite)
  
  # Theoretical moments
  theoretical_mean <- object$mean_theoretical
  theoretical_var <- object$var_theoretical
  theoretical_sd <- sqrt(abs(theoretical_var))
  
  list(
    simulated_values = orderstat_samples,
    n_sim = n_sim,
    n_finite = length(orderstat_finite),
    empirical_mean = empirical_mean,
    empirical_var = empirical_var,
    empirical_sd = empirical_sd,
    theoretical_mean = theoretical_mean,
    theoretical_var = theoretical_var,
    theoretical_sd = theoretical_sd,
    mean_error = abs(empirical_mean - theoretical_mean),
    var_error = abs(empirical_var - theoretical_var),
    comparison = data.frame(
      Statistic = c("Mean", "Variance", "Std Dev"),
      Empirical = c(empirical_mean, empirical_var, empirical_sd),
      Theoretical = c(theoretical_mean, theoretical_var, theoretical_sd),
      Error = c(
        abs(empirical_mean - theoretical_mean),
        abs(empirical_var - theoretical_var),
        abs(empirical_sd - theoretical_sd)
      )
    )
  )
}

