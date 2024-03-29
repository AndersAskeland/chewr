% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{autoplot_multisite_weight_loss}
\alias{autoplot_multisite_weight_loss}
\title{Creates plot comparing different participant groups from the Multisite study.
This function automatically compares weight loss}
\usage{
autoplot_multisite_weight_loss(comparison, df, exclude = TRUE, ...)
}
\arguments{
\item{comparison}{str | Variable to compare}

\item{exclude}{bool | If one should exclude participants that does not have 3 measurments.}

\item{...}{Extra parameters}
}
\description{
Creates plot comparing different participant groups from the Multisite study.
This function automatically compares weight loss
}
\examples{
autoplot_multisite_weight_loss("bmi", df)
}
