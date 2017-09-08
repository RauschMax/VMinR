\name{VD.computeShares}
\alias{VD.computeShares}
\title{Compute ValueDriver shares}
\usage{
VD.computeShares(design, utils, nlev, weight = NULL, FC = FALSE, dummy = TRUE)
}
\description{
Calculates the shares (first choice or preference share) for a ValueDriver like study.
}
\arguments{
\item{design}{A matrix containing the design including the concepts to simulate - in case it is not dummy coded use \code{dummy = TRUE} to ally dummy coding via \code{\link{convertSSItoDesign}}}

\item{utils}{A matrix containing the utilities}

\item{nlev}{A vector indicating the number of levels per attribute}

\item{weight}{A vector with the weights (one per respondent)}

\item{FC}{A boolean variable indicating if first choice simulation should be used (\code{FALSE} indicates preference share simulation) - default \code{TRUE}}

\item{dummy}{A boolean variable indicating if the design file needs to be dummy-coded using \code{\link{convertSSItoDesign}}. - default \{TRUE}
}
\value{
A list including elements
\item{meanShares}{aggregated shares accross all respondents}
\item{indShares}{individual shares for each respondent}
}
\author{Maximilian Rausch}
\examples{
sim_BaseR <- VD.computeShares(design = as.matrix(basecase_dummy),
                              utils = VDdata$utils_mat,
                              nlev = VDdata$nlev,
                              weight = VDdata$weight,
                              FC = FALSE)

round(sim_BaseR$meanShares, 3)
}

