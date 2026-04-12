readme_content <- '# StatDeriveR

**StatDeriveR** is an R package for step-by-step derivation and simulation-based verification in mathematical statistics.

## Overview

StatDeriveR is designed to connect mathematical statistics theory with computation. It takes probability and statistics problems, derives results step by step, and then validates them using Monte Carlo simulation.

Rather than only giving a final answer, **StatDeriveR**:

- shows the derivation process step by step
- presents key formulas clearly
- verifies results through simulation
- compares empirical and theoretical moments
- visualizes theoretical and simulated results

## Core Capabilities

### 1. Transformation of Random Variables

Derive the distribution of \( Y = g(X) \) for monotone transformations.

#### Supported transformations

- `Y = X^2`
- `Y = sqrt(X)`
- `Y = 1/X`
- `Y = log(X)`
- `Y = exp(X)`

#### Available distributions

The `dist` argument currently supports the following distributions:

- `"uniform"` with parameters `list(min = ..., max = ...)`
- `"exponential"` with parameters `list(rate = ...)`
- `"normal"` with parameters `list(mean = ..., sd = ...)`
- `"gamma"` with parameters `list(shape = ..., rate = ...)`
- `"beta"` with parameters `list(shape1 = ..., shape2 = ...)`

#### Example

```r
obj <- derive_transform(
  dist = "uniform",
  params = list(min = 0, max = 1),
  transform = "x^2"
)

print(obj)

obj <- simulate_check(obj, n_sim = 10000)

plot(obj)
