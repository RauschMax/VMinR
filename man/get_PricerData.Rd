\name{get_PricerData}
\alias{get_PricerData}
\title{Import ValuePricer data and definitions}
\usage{
get_PricerData(dat_file = NULL, def_file = NULL, nseg = NULL, none = TRUE)
}
\description{
Imports the dat/def files for a ValuePricer study and extracts the relevant information.
}
\arguments{
\item{dat_file}{A string value with the path to the DAT file to import.}

\item{def_file}{A string value with the path to the DEF file to import.}

\item{nseg}{An integer indicating the number of segments in the dat-file}

\item{none}{A boolean variable indicating whether NONE is included in the dat file or not - default = \code{TRUE}}
}
\value{
A list including elements
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
\author{Maximilian Rausch - Maximilian.Rausch@tns-infratest.com}
\examples{
\dontrun{
beer_data <- get_PricerData(dat_file = "beer_study.dat",
                            def_file = "beer_study.def",
                            nseg = 42,
                            none = TRUE)

str(beer_data)
}
}

