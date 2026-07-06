# miBMA: Multiple Imputation under Bayesian Model Averaging

**miBMA** is an R package that addresses **imputation model uncertainty** in missing data problems by incorporating **Bayesian model averaging (BMA)** into the multiple imputation process.

This is the companion software to:

> Kaplan, D., & Yavuz, S. (2020). An Approach to Addressing Multiple Imputation Model Uncertainty Using Bayesian Model Averaging. *Multivariate Behavioral Research*, 55(4), 553–567. https://doi.org/10.1080/00273171.2019.1657790

## The idea

So-called *Bayesianly proper* approaches to multiple imputation correctly account for uncertainty in the imputation model **parameters**, but ignore uncertainty in the imputation **model itself**. miBMA accounts for both by applying Bayesian model averaging within the imputation step, implemented under the fully conditional specification (FCS) approach.

The package plugs directly into [`mice`](https://cran.r-project.org/package=mice) as a custom imputation method: `method = "miBMA"`.

## Installation

```r
# install.packages("devtools")
devtools::install_github("Sinan-Yavuz/miBMA")
```

## Usage

```r
library(miBMA)
library(mice)

# data: a matrix/data.frame with missing values
imp <- mice(data, method = "miBMA", m = 20, maxit = 10)

# analyze completed datasets as usual
md.pattern(complete(imp, 1))
```

See [`Example.R`](Example.R) for a full worked example that simulates correlated multivariate data, imposes 40% MAR missingness, and imputes with miBMA.

## When to use it

Consider miBMA when:

- You have missing data under MAR and are unsure which covariates belong in the imputation model
- You want imputation-model uncertainty propagated into your final estimates
- Standard `mice` results appear sensitive to imputation model specification

## Citing

If you use miBMA, please cite the *Multivariate Behavioral Research* paper above.

```bibtex
@article{KaplanYavuz2020,
  author  = {Kaplan, David and Yavuz, Sinan},
  title   = {An Approach to Addressing Multiple Imputation Model Uncertainty Using Bayesian Model Averaging},
  journal = {Multivariate Behavioral Research},
  year    = {2020},
  volume  = {55},
  number  = {4},
  pages   = {553--567},
  doi     = {10.1080/00273171.2019.1657790}
}
```

## Authors

- **Sinan Yavuz** ([ORCID](https://orcid.org/0000-0003-3131-7820)) — author, maintainer
- **David Kaplan** — author

## License

GPL-3
