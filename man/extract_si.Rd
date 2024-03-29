% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ScientificNotation.R
\name{extract_si}
\alias{extract_si}
\title{Extract SI units from a given expression}
\usage{
extract_si(unit)
}
\arguments{
\item{unit}{A character string containing the expression with a numeric value and a unit.}
}
\value{
A character string representing the SI unit extracted from the expression.
}
\description{
This function extracts the SI units from a given expression containing a numeric value and a unit.
}
\examples{
extract_si("m")
# Output: "m"
extract_si("km")
# Output: "km"
extract_si("") # No unit provided
# Output: ""
}
