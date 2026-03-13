# ============================================
# README.md
# ============================================

readme_content <- '# StatDeriveR

A personal R package for step-by-step derivation and simulation verification in mathematical statistics.

## Overview

**StatDeriveR** is an R package that bridges the gap between mathematical statistics theory and computational practice. It takes mathematical problems from probability and statistics, derives the results step by step, and validates them through Monte Carlo simulation.

Rather than just computing answers, StatDeriveR:
- Shows the mathematical derivation steps
- Presents formulas clearly
- Verifies results through simulation
- Compares empirical and theoretical moments
- Visualizes theory vs simulation

## Core Capabilities

### 1. Transformation of Random Variables
Derive the distribution of Y = g(X) for monotone transformations.

**Supported transformations:**
- Y = X^2
- Y = sqrt(X)
- Y = 1/X
- Y = log(X)
- Y = exp(X)

**Example:**
```r
obj <- derive_transform(
  dist = "uniform",
  params = list(min = 0, max = 1),
  transform = "x^2"
)
print(obj)
simulate_check(obj, n_sim = 10000)
plot(obj)
