Lab 8

Overview

This lab demonstrates the use of Monte Carlo simulation techniques in R to estimate expectations and variances of transformed random variables. It also compares different simulation approaches for computational efficiency.

Objectives

Implement inverse transform sampling
Estimate 𝐸(𝑋^5) and  Var(𝑋^5)
Work with truncated distributions
Compare simulation methods for efficiency
Understand variance reduction techniques

Observations

Larger sample sizes improve accuracy (Law of Large Numbers)
Monte Carlo estimates stabilize as 𝑁 increases
Inverse transform sampling is effective for non-standard distributions
Alternative formulations can significantly improve performance

Notes

set.seed(123) ensures reproducibility
Code is modular and reusable
Suitable for extension to other distributions