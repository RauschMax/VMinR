\name{VMinR}
\alias{VMinR}
\docType{data}
\title{VM test data - timtim}
\description{Dataset containing the variable contents (data model) settings to perform input validation.}
\usage{data("VMinR")}
\format{
  A list with 3 elements.
  \describe{
   \item{VDdata}{A list containing the output of \code{\link{get_DriverData}}}
   \item{basecase}{A matrix containing the scenario information in SSI style}
   \item{basecase_dummy_2}{A matrix containing the scenario information in dummy coding}
  }
}
\examples{
data(VMinR)
str(VMinR)
}
\keyword{datasets}
