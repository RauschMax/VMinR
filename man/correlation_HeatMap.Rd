\name{correlation_HeatMap}
\alias{correlation_HeatMap}
\title{Correlation heatmap}
\usage{
correlation_HeatMap(input_file = NULL, sepOUT = ";", decOUT = ",",
                    usedraws = FALSE, clustered = FALSE)
}
\description{
Calculates correlation heatmap and MDS coordinates for ValuePricer projects.
}
\arguments{
\item{input_file}{the path to the heatmap input file. A specific file which contains all the necessary information. (See VM sharepoint)}
\item{sepOUT}{separator for output csv-files - default = \code{";"}}
\item{decOUT}{decimal sign for output csv-files - default = \code{","}}
\item{usedraws}{A boolean variable indicating whether DRAWS should be used as well or not - default = \code{FALSE}}
\item{clustered}{A boolean variable indicating whether clustered heatmaps should be calculated or not - default = \code{FALSE}}
}
\value{
A list including 23 elements
\item{input_file}{the path to the heatmap input file.}
\item{cor}{A matrix of the correlations for the scenario specified in the input file.}
\item{cor_draws}{A matrix of the correlations for the scenario specified in the input file based on the DRAWS. If \code{usedraws == TRUE}}
\item{cor_clustered}{A matrix of the correlations for the scenario specified in the input file for the CLUSTERS. If \code{clustered == TRUE}}
\item{cor_draws_clustered}{A matrix of the correlations for the scenario specified in the input file for the CLUSTERS based on the DRAWS If \code{usedraws == TRUE} and \code{clustered == TRUE}}
\item{base_sim}{A vector with the unweighted aggregated shares for the scenario specified in the input file.}
\item{base_sim_draws}{A vector with the unweighted aggregated shares for the scenario specified in the input file based on the DRAWS. If \code{usedraws == TRUE}}
\item{draws}{A matrix containing the draws used for the study}
\item{utls}{A matrix containing the utilities used for the study}
\item{Xbeta}{\code{X * beta} matrix (utility sums).}
\item{Xbeta_draws}{\code{X * beta} matrix for the draws (utility sums). If \code{usedraws == TRUE}}
\item{Xbeta_clustered}{\code{X * beta} matrix (utility sums). If \code{clustered == TRUE}}
\item{Xbeta_draws_clustered}{\code{X * beta} matrix for the draws (utility sums). If \code{usedraws == TRUE} and \code{clustered == TRUE}}
\item{brand_list}{A list with one item per cluster containing the respectiv indicies for the SKUs of the clusters.}
\item{MDS_coord}{A matrix containing the MDS coordinates}
\item{MDS_coord_draws}{A matrix containing the MDS coordinates for DRAWS. If \code{usedraws == TRUE}}
\item{MDS_coord_clustered}{A matrix containing the clustered MDS coordinates. If \code{clustered == TRUE}}
\item{MDS_coord_draws_clustered}{A matrix containing the clustered MDS coordinates for DRAWS. If \code{usedraws == TRUE} and \code{clustered == TRUE}}
\item{SKUlabels}{A vector with the SKU labels passed by the input file.}
\item{ClusterLabels}{A vector with the cluster labels passed by the input file.}
\item{prices}{A matrix containing the prices used in the model.}
\item{simPrices}{A vector containing the prices used for the heatmap calculation.}
\item{simSKUs}{A vector with the SKU indicies used for the heatmap calculation.}
}
\author{Maximilian Rausch - Maximilian.Rausch@tns-infratest.com}
\examples{
\dontrun{
heat <- correlation_HeatMap("Input_DUMMY.txt",
                            usedraws=FALSE,
                            clustered = FALSE,
                            sepOUT=",",
                            decOUT=".")
}
}

