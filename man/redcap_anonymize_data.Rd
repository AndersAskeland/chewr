% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap.R
\name{redcap_anonymize_data}
\alias{redcap_anonymize_data}
\title{Replaces data in RedCap df with random data.}
\usage{
redcap_anonymize_data(df, remove = NULL)
}
\arguments{
\item{df}{tibble | Data frame that contains data exported from RedCap using
\code{redcap_export()}.}

\item{remove}{vec | Vector of variables to be completely removed. Default: NULL}
}
\description{
Replaces data in RedCap df with random data.
}
