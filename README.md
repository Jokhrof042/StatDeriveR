# StatDeriveR

<!-- badges: start -->
<!-- Add CRAN / status badges here later -->
<!-- badges: end -->

**StatDeriveR** is an R package for **step-by-step derivation** and **simulation-based verification** in mathematical statistics.

It is designed to help users move from theoretical results to computational understanding. Rather than only returning final answers, the package explains important derivations in a structured way and then checks them using Monte Carlo simulation.

This makes the package useful for:

- students learning mathematical statistics
- instructors teaching derivations and distributions
- researchers who want quick computational verification of theoretical results

---

## Overview

Mathematical statistics often requires deriving distributions, expectations, and related results by hand. These derivations are conceptually important, but they can also be difficult to follow and verify.

**StatDeriveR** aims to bridge that gap by combining:

- **theoretical derivation**
- **clear structured output**
- **simulation-based checking**
- **visual comparison of theory and empirical behavior**

The package currently focuses on:

- transformations of random variables
- order statistics
- simulation verification of derived results

---

## Main Features

### 1. Transformation of Random Variables
Derive the distribution of a transformed variable \( Y = g(X) \) for supported distributions and transformations.

Examples of supported transformations include:

- \( Y = X^2 \)
- \( Y = \sqrt{X} \)
- \( Y = 1/X \)
- \( Y = \log(X) \)
- \( Y = e^X \)

### 2. Order Statistics
Obtain symbolic derivation of the density of the \(k\)-th order statistic from a sample of size \(n\).

### 3. Simulation Verification
Compare theoretical results with Monte Carlo simulation to see whether the derived result behaves as expected in practice.

### 4. Print and Plot Methods
Display derivation results clearly and visualize theoretical versus simulated behavior.

---

## Installation

Install the released version from CRAN:

```r
install.packages("StatDeriveR")
