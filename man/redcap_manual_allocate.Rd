% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap.R
\name{redcap_manual_allocate}
\alias{redcap_manual_allocate}
\title{Takes a redcap export file as input and changes redcap arm on participant}
\usage{
redcap_manual_allocate(
  file_location,
  export_dir = "/Users/andersaskeland/Documents/Multisite (Local)/5 - Redcap/Import/"
)
}
\arguments{
\item{file_location}{str | File location path}

\item{export_dir}{str | Export file path}
}
\value{
None
}
\description{
Takes a redcap export file as input and changes redcap arm on participant
}
\examples{
redcap_manual_allocate(file_location = "/home/test.csv")
}
