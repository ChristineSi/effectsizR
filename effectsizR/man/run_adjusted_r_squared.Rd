% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_adjusted_squared_R.R
\name{run_adjusted_r_squared}
\alias{run_adjusted_r_squared}
\title{Wizard assisted calculation of adjusted R squared for univariate models}
\usage{
run_adjusted_r_squared()
}
\arguments{
\item{N}{number of subjects in the tested model}

\item{adjusted_r_squared}{variance of data which is accounted for by tested model}

\item{k_predictors}{number of predictors in the model (not including the intercept)}
}
\value{
adjusted R squared for univariate models
}
\description{
<R2 represents the variance of the data that is accounted for by
a particular model and should be used for predictors in univariate models only
(for a particular predictor in multivariate models, we recommend effect sizes
estimated from t values or raw data). Often, the squareroot of R2 is used as an
effect statistic in meta-analysis when models include one predictor, and even if
they include more than one predictor. As the square-root of R2 provides a biased
 effect-size estimate of a predictor of interest and as this bias is particularly
  severe if sample size is small, we corrected for this bias.>
}
