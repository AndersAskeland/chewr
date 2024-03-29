% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap.R
\name{redcap_export_field_list}
\alias{redcap_export_field_list}
\title{Extract field list from a redcap project.
API token is collected using R studio api when function is run (Avoids
collecting API token in Rhistory).}
\usage{
redcap_export_field_list(redcap_uri = "https://redcap.rn.dk/api/", ...)
}
\arguments{
\item{redcap_uri}{str | URL to redcap API. Defaults to RN server.}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> Extra arguments.}
}
\value{
tibble
}
\description{
Extract field list from a redcap project.
API token is collected using R studio api when function is run (Avoids
collecting API token in Rhistory).
}
\examples{
# Extracts all code's from redcap and store them in tibble
df <- redap_codebook()
}
