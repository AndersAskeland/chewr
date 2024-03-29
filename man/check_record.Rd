% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{check_record}
\alias{check_record}
\title{Checks if participant ID already exist inside given redcap arm.}
\usage{
check_record(
  participant_id,
  enrolment_arm,
  url = "https://redcap.rn.dk/api/",
  ...
)
}
\arguments{
\item{participant_id}{str}

\item{enrolment_arm}{str}

\item{url}{str}

\item{...}{}
}
\value{
bool
}
\description{
Checks if participant ID already exist inside given redcap arm.
}
\examples{
check_record("3030", "enrolment_arm_1")
}
