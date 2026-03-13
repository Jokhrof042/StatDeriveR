
#' Get PDF function for a distribution
#'
#' @param dist Character name of distribution
#' @param params List of distribution parameters
#' @return A function that evaluates the pdf
#' @keywords internal
.pdf_eval <- function(dist, params) {
  switch(dist,
    uniform = function(x) dunif(x, params$min, params$max),
    exponential = function(x) dexp(x, params$rate),
    normal = function(x) dnorm(x, params$mean, params$sd),
    gamma = function(x) dgamma(x, params$shape, params$rate),
    beta = function(x) dbeta(x, params$shape1, params$shape2),
    stop("Unknown distribution: ", dist)
  )
}

#' Get CDF function for a distribution
#'
#' @param dist Character name of distribution
#' @param params List of distribution parameters
#' @return A function that evaluates the cdf
#' @keywords internal
.cdf_eval <- function(dist, params) {
  switch(dist,
    uniform = function(x) punif(x, params$min, params$max),
    exponential = function(x) pexp(x, params$rate),
    normal = function(x) pnorm(x, params$mean, params$sd),
    gamma = function(x) pgamma(x, params$shape, params$rate),
    beta = function(x) pbeta(x, params$shape1, params$shape2),
    stop("Unknown distribution: ", dist)
  )
}

#' Get random number generator
#'
#' @param dist Character name of distribution
#' @param params List of distribution parameters
#' @param n Number of observations
#' @return Vector of random values
#' @keywords internal
.rng_eval <- function(dist, params, n) {
  switch(dist,
    uniform = runif(n, params$min, params$max),
    exponential = rexp(n, params$rate),
    normal = rnorm(n, params$mean, params$sd),
    gamma = rgamma(n, params$shape, params$rate),
    beta = rbeta(n, params$shape1, params$shape2),
    stop("Unknown distribution: ", dist)
  )
}

#' Get support of a distribution
#'
#' @param dist Character name of distribution
#' @param params List of distribution parameters
#' @return List with lower and upper bounds
#' @keywords internal
.support_eval <- function(dist, params) {
  switch(dist,
    uniform = list(lower = params$min, upper = params$max),
    exponential = list(lower = 0, upper = Inf),
    normal = list(lower = -Inf, upper = Inf),
    gamma = list(lower = 0, upper = Inf),
    beta = list(lower = 0, upper = 1),
    stop("Unknown distribution: ", dist)
  )
}

#' Format distribution for printing
#'
#' @param dist Distribution name
#' @param params Parameters
#' @return Formatted string
#' @keywords internal
.format_dist <- function(dist, params) {
  switch(dist,
    uniform = sprintf("Uniform(%.2f, %.2f)", params$min, params$max),
    exponential = sprintf("Exponential(rate=%.2f)", params$rate),
    normal = sprintf("N(%.2f, %.2f²)", params$mean, params$sd),
    gamma = sprintf("Gamma(%.2f, %.2f)", params$shape, params$rate),
    beta = sprintf("Beta(%.2f, %.2f)", params$shape1, params$shape2),
    stop("Unknown distribution: ", dist)
  )
}

#' Default parameters for distribution
#'
#' @param dist Distribution name
#' @return List of default parameters
#' @keywords internal
.default_params <- function(dist) {
  switch(dist,
    uniform = list(min = 0, max = 1),
    exponential = list(rate = 1),
    normal = list(mean = 0, sd = 1),
    gamma = list(shape = 2, rate = 1),
    beta = list(shape1 = 2, shape2 = 2),
    stop("Unknown distribution: ", dist)
  )
}

