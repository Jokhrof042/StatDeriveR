# ============================================
# STEP 3: Create R/transforms.R
# ============================================

# Create the transforms.R file
transforms_code <- '
#\' Derive the distribution of a transformed random variable
#\'
#\' @param dist Character: base distribution name
#\' @param params List: distribution parameters
#\' @param transform Character: transformation (e.g., "x^2", "sqrt(x)", "1/x")
#\' @param support Optional vector c(lower, upper) for original support
#\'
#\' @return Object of class "statderive" with derivation steps
#\' @export
#\'
#\' @examples
#\' # Transform Y = X^2 where X ~ Uniform(0,1)
#\' obj <- derive_transform(
#\'   dist = "uniform",
#\'   params = list(min = 0, max = 1),
#\'   transform = "x^2"
#\' )
derive_transform <- function(dist, params, transform, support = NULL) {
  
  # Validate distribution
  if (!dist %in% c("uniform", "exponential", "normal", "gamma", "beta")) {
    stop("Distribution not supported: ", dist)
  }
  
  # Validate transformation
  valid_transforms <- c("x^2", "sqrt(x)", "1/x", "log(x)", "exp(x)")
  if (!transform %in% valid_transforms) {
    stop("Transformation not supported. Use one of: ",
         paste(valid_transforms, collapse = ", "))
  }
  
  # Get support if not provided
  if (is.null(support)) {
    support <- .support_eval(dist, params)
    support <- c(support$lower, support$upper)
  }
  
  # Derive based on transformation
  derivation <- switch(transform,
    "x^2" = .derive_x_squared(dist, params, support),
    "sqrt(x)" = .derive_sqrt_x(dist, params, support),
    "1/x" = .derive_inv_x(dist, params, support),
    "log(x)" = .derive_log_x(dist, params, support),
    "exp(x)" = .derive_exp_x(dist, params, support)
  )
  
  # Build result object
  result <- list(
    type = "transform",
    dist = dist,
    params = params,
    transform = transform,
    original_support = support,
    inverse = derivation$inverse,
    derivative = derivation$derivative,
    new_support = derivation$new_support,
    pdf_formula = derivation$pdf_formula,
    steps = derivation$steps,
    assumptions = derivation$assumptions,
    pdf_function = derivation$pdf_function,
    simulation = NULL
  )
  
  class(result) <- "statderive"
  return(result)
}

# ============================================
# Internal Derivation Functions
# ============================================

#\' Derive Y = X^2
#\' @keywords internal
.derive_x_squared <- function(dist, params, support) {
  list(
    inverse = "x = sqrt(y)",
    derivative = "dx/dy = 1/(2*sqrt(y))",
    new_support = c(support[1]^2, support[2]^2),
    pdf_formula = "f_Y(y) = f_X(sqrt(y)) * 1/(2*sqrt(y))",
    steps = c(
      "Define Y = X^2",
      "Since X in support, Y in [X_min^2, X_max^2]",
      "Invert: X = sqrt(Y)",
      "Compute derivative: dX/dY = 1/(2*sqrt(Y))",
      "Apply change-of-variable formula: f_Y(y) = f_X(sqrt(y)) * |dX/dY|"
    ),
    assumptions = c(
      "X is continuous",
      "X is positive on its support or support is transformed appropriately"
    ),
    pdf_function = function(y) {
      pdf_x <- .pdf_eval(dist, params)
      x <- sqrt(y)
      pdf_x(x) * 1 / (2 * sqrt(y))
    }
  )
}

#\' Derive Y = sqrt(X)
#\' @keywords internal
.derive_sqrt_x <- function(dist, params, support) {
  list(
    inverse = "x = y^2",
    derivative = "dx/dy = 2*y",
    new_support = c(sqrt(support[1]), sqrt(support[2])),
    pdf_formula = "f_Y(y) = f_X(y^2) * 2*y",
    steps = c(
      "Define Y = sqrt(X)",
      "Invert: X = Y^2",
      "New support: Y in [sqrt(X_min), sqrt(X_max)]",
      "Compute derivative: dX/dY = 2*Y",
      "Apply change-of-variable formula: f_Y(y) = f_X(y^2) * 2*y"
    ),
    assumptions = c(
      "X is continuous",
      "X has non-negative support"
    ),
    pdf_function = function(y) {
      pdf_x <- .pdf_eval(dist, params)
      x <- y^2
      pdf_x(x) * 2 * y
    }
  )
}

#\' Derive Y = 1/X
#\' @keywords internal
.derive_inv_x <- function(dist, params, support) {
  list(
    inverse = "x = 1/y",
    derivative = "dx/dy = -1/y^2",
    new_support = c(1/support[2], 1/support[1]),
    pdf_formula = "f_Y(y) = f_X(1/y) * 1/y^2",
    steps = c(
      "Define Y = 1/X",
      "Invert: X = 1/Y",
      "New support: Y in [1/X_max, 1/X_min] (note reversal)",
      "Compute derivative: dX/dY = -1/Y^2",
      "Apply change-of-variable formula: f_Y(y) = f_X(1/y) * |dX/dY| = f_X(1/y) * 1/y^2"
    ),
    assumptions = c(
      "X is continuous",
      "X does not include zero in its support"
    ),
    pdf_function = function(y) {
      pdf_x <- .pdf_eval(dist, params)
      x <- 1 / y
      pdf_x(x) * 1 / (y^2)
    }
  )
}

#\' Derive Y = log(X)
#\' @keywords internal
.derive_log_x <- function(dist, params, support) {
  list(
    inverse = "x = exp(y)",
    derivative = "dx/dy = exp(y)",
    new_support = c(log(support[1]), log(support[2])),
    pdf_formula = "f_Y(y) = f_X(exp(y)) * exp(y)",
    steps = c(
      "Define Y = log(X)",
      "Invert: X = exp(Y)",
      "New support: Y in [log(X_min), log(X_max)]",
      "Compute derivative: dX/dY = exp(Y)",
      "Apply change-of-variable formula: f_Y(y) = f_X(exp(y)) * exp(y)"
    ),
    assumptions = c(
      "X is continuous",
      "X has positive support"
    ),
    pdf_function = function(y) {
      pdf_x <- .pdf_eval(dist, params)
      x <- exp(y)
      pdf_x(x) * exp(y)
    }
  )
}

#\' Derive Y = exp(X)
#\' @keywords internal
.derive_exp_x <- function(dist, params, support) {
  list(
    inverse = "x = log(y)",
    derivative = "dx/dy = 1/y",
    new_support = c(exp(support[1]), exp(support[2])),
    pdf_formula = "f_Y(y) = f_X(log(y)) * 1/y",
    steps = c(
      "Define Y = exp(X)",
      "Invert: X = log(Y)",
      "New support: Y in [exp(X_min), exp(X_max)]",
      "Compute derivative: dX/dY = 1/Y",
      "Apply change-of-variable formula: f_Y(y) = f_X(log(y)) * 1/y"
    ),
    assumptions = c(
      "X is continuous",
      "X has real-valued support"
    ),
    pdf_function = function(y) {
      pdf_x <- .pdf_eval(dist, params)
      x <- log(y)
      pdf_x(x) * 1 / y
    }
  )
}
'

# Write the file
writeLines(transforms_code, "R/transforms.R")

cat("✅ Created R/transforms.R\n")
cat("File location:", file.path(getwd(), "R/transforms.R"), "\n")
cat("File size:", file.size("R/transforms.R"), "bytes\n")

# Verify the file was created
if (file.exists("R/transforms.R")) {
  cat("\n✅ SUCCESS! File created and verified.\n")
  cat("\nFunctions created:\n")
  cat("  • derive_transform() - Main function (EXPORTED)\n")
  cat("  • .derive_x_squared() - Derive Y = X^2\n")
  cat("  • .derive_sqrt_x() - Derive Y = sqrt(X)\n")
  cat("  • .derive_inv_x() - Derive Y = 1/X\n")
  cat("  • .derive_log_x() - Derive Y = log(X)\n")
  cat("  • .derive_exp_x() - Derive Y = exp(X)\n")
  cat("\nSupported transformations:\n")
  cat("  ✓ x^2\n")
  cat("  ✓ sqrt(x)\n")
  cat("  ✓ 1/x\n")
  cat("  ✓ log(x)\n")
  cat("  ✓ exp(x)\n")
} else {
  cat("\n❌ ERROR: File not created. Check your working directory.\n")
}