% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anovaTest.R
\name{fastanova_test}
\alias{fastanova_test}
\title{A pipe friendly wrapper for one way ANOVA}
\usage{
fastanova_test(data, x, include = "p-value", transform_func = NULL)
}
\arguments{
\item{data}{The data frame containing variables to be analyzed}

\item{x}{The independent or predictor variable in the data}

\item{include}{a vector containing ANOVA statistical result to include to the final data frame - `f-value` or/and `p-value` defaults to only `p-value` # nolint}

\item{transform_func}{a callback that can be to transorm each dependent var in the dataset # nolint}
}
\value{
dataframe containing the ANOVA result
}
\description{
It carries out one way ANOVA and is pipe friendly
}
