# ============================================
# ALTERNATIVE: Manual package creation
# ============================================

# We'll skip devtools and create manually



# Create directory manually
if (!dir.exists("StatDeriveR")) {
  dir.create("StatDeriveR")
}

#setwd("~/Desktop/StatDeriveR")

# Create R folder
if (!dir.exists("R")) {
  dir.create("R")
}

# Create DESCRIPTION file manually
description_text <- "Package: StatDeriveR
Title: Step-by-Step Derivation and Simulation Verification in Mathematical Statistics
Version: 0.1.0
Authors@R: c(
  person('Your Name', 'Here', email = 'your.email@example.com', role = c('aut', 'cre'))
)
Description: A package for deriving core mathematical-statistics results step by step.
License: MIT + file LICENSE
Encoding: UTF-8
Language: en-US
Depends: R (>= 4.0.0)
Imports: stats, graphics, methods
"

writeLines(description_text, "DESCRIPTION")

# Create NAMESPACE file manually
namespace_text <- "# Exports
export(derive_transform)
export(derive_orderstat)
export(simulate_check)

# S3 methods
S3method(print, statderive)
S3method(plot, statderive)

# Imports
import(stats)
import(graphics)
import(methods)
"

writeLines(namespace_text, "NAMESPACE")

# Create .Rbuildignore
rbuildignore_text <- "^.*\\.Rproj$
^.*\\.Rproj\\.user$
^README\\.md$
^LICENSE.*$
"

writeLines(rbuildignore_text, ".Rbuildignore")

cat("✅ Package created manually!\n")
cat("Directory:", getwd(), "\n")
cat("Files:\n")
print(list.files(all.files = TRUE))



# ============================================
# STEP 2: Create R/distributions.R
# ============================================


# Create the distributions.R file
distributions_code <- '
#\' Get PDF function for a distribution
#\'
#\' @param dist Character name of distribution
#\' @param params List of distribution parameters
#\' @return A function that evaluates the pdf
#\' @keywords internal
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

#\' Get CDF function for a distribution
#\'
#\' @param dist Character name of distribution
#\' @param params List of distribution parameters
#\' @return A function that evaluates the cdf
#\' @keywords internal
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

#\' Get random number generator
#\'
#\' @param dist Character name of distribution
#\' @param params List of distribution parameters
#\' @param n Number of observations
#\' @return Vector of random values
#\' @keywords internal
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

#\' Get support of a distribution
#\'
#\' @param dist Character name of distribution
#\' @param params List of distribution parameters
#\' @return List with lower and upper bounds
#\' @keywords internal
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

#\' Format distribution for printing
#\'
#\' @param dist Distribution name
#\' @param params Parameters
#\' @return Formatted string
#\' @keywords internal
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

#\' Default parameters for distribution
#\'
#\' @param dist Distribution name
#\' @return List of default parameters
#\' @keywords internal
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
'

# Write the file
writeLines(distributions_code, "R/distributions.R")

cat("✅ Created R/distributions.R\n")
cat("File location:", file.path(getwd(), "R/distributions.R"), "\n")
cat("File size:", file.size("R/distributions.R"), "bytes\n")

# Verify the file was created
if (file.exists("R/distributions.R")) {
  cat("\n✅ SUCCESS! File created and verified.\n")
  cat("\nFunctions created:\n")
  cat("  • .pdf_eval() - Get PDF function\n")
  cat("  • .cdf_eval() - Get CDF function\n")
  cat("  • .rng_eval() - Generate random samples\n")
  cat("  • .support_eval() - Get support bounds\n")
  cat("  • .format_dist() - Format distribution names\n")
  cat("  • .default_params() - Default parameters\n")
} else {
  cat("\n❌ ERROR: File not created. Check your working directory.\n")
}