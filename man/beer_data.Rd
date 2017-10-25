\name{beer_data}
\alias{beer_data}
\docType{data}
\title{VM test data - ValuePricer "beer study""}
\description{Dataset containing the variable contents (data model) settings to perform input validation.}
\usage{data("beer_data")}
\format{
  A list with the data of teh beer study.
  \describe{
    \item{dat}{A matrix of the imported dat-file}
    \item{utils_mat}{A matrix including the utilities from the dat file}
    \item{utils_list}{A list including the individual utilities from the dat file. One list element per respondent}
    \item{iaw}{A matrix including the individual awareness factors from the dat file.}
    \item{idis}{A matrix including the individual distribution factors from the dat file.}
    \item{seg}{A matrix containing the segment data}
    \item{weight}{A vector containing the weight per respondent}
    \item{def}{A list containing the values from \code{\link{VD.read_def}}}
    \item{pricemat_tested}{A matrix containing the prices used in the model}
    \item{pricerange_tested}{A vector containing the minimal and maximal price used in the model}
    \item{pr_range_mat}{A matrix containing the minimal and maximal prices per SKU used in the model}
    \item{SKUs}{A vector containing the SKU labels from the def-file.}
    \item{nlev}{A vector containing the number of levels per attribute.}
    \item{ID}{A vector containing the ID per respondent}
  }
}
\examples{
data(beer_data)
str(beer_data)
}
\keyword{datasets}
