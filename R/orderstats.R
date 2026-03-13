
#' Derive the distribution of an order statistic
#'
#' @param dist Character: base distribution name
#' @param params List: distribution parameters
#' @param n Integer: sample size
#' @param r Integer: order (1 = min, n = max)
#'
#' @return Object of class "statderive" with order statistic derivation
#' @export
#'
#' @examples
#' # Minimum of 5 exponential(rate=2) samples
#' obj <- derive_orderstat(
#'   dist = "exponential",
#'   params = list(rate = 2),
#'   n = 5,
#'   r = 1
#' )
derive_orderstat <- function(dist, params, n, r) {
  
  # Validate inputs
  if (!dist %in% c("uniform", "exponential", "normal", "gamma", "beta")) {
    stop("Distribution not supported: ", dist)
  }
  
  if (r < 1 || r > n) {
    stop("Order r must be between 1 and n")
  }
  
  # Get pdf and cdf
  pdf_x <- .pdf_eval(dist, params)
  cdf_x <- .cdf_eval(dist, params)
  support <- .support_eval(dist, params)
  
  # Compute order statistic formula
  formula_str <- sprintf(
    "f_X(%d)(x) = %d! / ((%d-1)! * (%d-%d)!) * [F(x)]^(%d-1) * [1-F(x)]^(%d-%d) * f(x)",
    r, n, r, n, r, r, n, r
  )
  
  # Simplify for special cases
  if (r == 1) {
    formula_str <- sprintf(
      "f_X(1)(x) = %d * [1-F(x)]^(%d-1) * f(x)  [minimum]",
      n, n
    )
  } else if (r == n) {
    formula_str <- sprintf(
      "f_X(%d)(x) = %d * [F(x)]^(%d-1) * f(x)  [maximum]",
      n, n, n
    )
  }
  
  # Create pdf function for order statistic
  const <- factorial(n) / (factorial(r - 1) * factorial(n - r))
  
  pdf_orderstat <- function(x) {
    const * (cdf_x(x)^(r - 1)) * ((1 - cdf_x(x))^(n - r)) * pdf_x(x)
  }
  
  # Create CDF function
  cdf_orderstat <- function(x) {
    F_x <- cdf_x(x)
    pbeta(F_x, r, n - r + 1)
  }
  
  # Compute theoretical mean and variance
  mean_theoretical <- .compute_orderstat_mean(dist, params, n, r)
  var_theoretical <- .compute_orderstat_var(dist, params, n, r)
  
  # Build result
  result <- list(
    type = "orderstat",
    dist = dist,
    params = params,
    n = n,
    r = r,
    formula = formula_str,
    pdf_function = pdf_orderstat,
    cdf_function = cdf_orderstat,
    support = c(support$lower, support$upper),
    steps = c(
      sprintf("Consider random sample X_1, ..., X_%d from %s", n, .format_dist(dist, params)),
      "Order the sample: X_(1) <= X_(2) <= ... <= X_(n)",
      sprintf("We want the distribution of X_(%d)", r),
      sprintf("General formula: f_X(%d)(x) = (n! / ((%d-1)! * (%d-%d)!)) * [F(x)]^%d * [1-F(x)]^%d * f(x)",
              r, r, n, r, r-1, n-r),
      "Integrate to find CDF and moments"
    ),
    assumptions = c(
      "X_1, ..., X_n are i.i.d.",
      "X is continuous"
    ),
    mean_theoretical = mean_theoretical,
    var_theoretical = var_theoretical,
    simulation = NULL
  )
  
  class(result) <- "statderive"
  return(result)
}

#' Compute theoretical mean of order statistic
#' @keywords internal
.compute_orderstat_mean <- function(dist, params, n, r) {
  support <- .support_eval(dist, params)
  cdf_x <- .cdf_eval(dist, params)
  pdf_x <- .pdf_eval(dist, params)
  
  const <- factorial(n) / (factorial(r - 1) * factorial(n - r))
  
  integrand <- function(x) {
    x * const * (cdf_x(x)^(r - 1)) * ((1 - cdf_x(x))^(n - r)) * pdf_x(x)
  }
  
  tryCatch(
    integrate(integrand, support$lower, support$upper)$value,
    error = function(e) NA
  )
}

#' Compute theoretical variance of order statistic
#' @keywords internal
.compute_orderstat_var <- function(dist, params, n, r) {
  mean_r <- .compute_orderstat_mean(dist, params, n, r)
  if (is.na(mean_r)) return(NA)
  
  support <- .support_eval(dist, params)
  cdf_x <- .cdf_eval(dist, params)
  pdf_x <- .pdf_eval(dist, params)
  
  const <- factorial(n) / (factorial(r - 1) * factorial(n - r))
  
  integrand <- function(x) {
    (x^2) * const * (cdf_x(x)^(r - 1)) * ((1 - cdf_x(x))^(n - r)) * pdf_x(x)
  }
  
  E_X2 <- tryCatch(
    integrate(integrand, support$lower, support$upper)$value,
    error = function(e) NA
  )
  
  if (is.na(E_X2)) return(NA)
  E_X2 - mean_r^2
}

