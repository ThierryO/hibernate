% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/display_importance.R
\name{display_importance}
\alias{display_importance}
\title{Display the importance of the spaces based on a model}
\usage{
display_importance(
  model,
  labels = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 0.75, 1, 1.33, 2, 5, 10, 20, 50, 100),
  crs = 31370,
  legend_title = "relative\\nimportance"
)
}
\arguments{
\item{model}{the output of \code{ìmputation_model()}.}

\item{labels}{the labels for the relative changes.}

\item{crs}{target coordinate reference system: object of class `crs`, or input string for \link[sf]{st_crs}}

\item{legend_title}{title for the colour legend}
}
\description{
Display the importance of the spaces based on a model
}
