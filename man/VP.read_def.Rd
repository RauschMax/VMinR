\name{VP.read_def}
\alias{VP.read_def}
\title{Read ValuePricer definitions file}
\usage{
VP.read_def(file)
}
\description{
Reads the ValuePricer def file extracting the labels and prices
}
\arguments{
\item{file}{A string value with the path to the DEF file to import.}
}
\value{
A list including elements
\item{brands}{A vector including the SKU labels}
\item{prices}{A matrix (nSKUs x nPrices) including the prices per SKU}
}
\author{Maximilian Rausch - Maximilian.Rausch@tns-infratest.com}
\examples{
\dontrun{
VP.read_def(file = "data/TEST_FILE.def")
}
}

