# function computes shares for a ValueDriver study - First Choice and Preference Share



#' Compute ValueDriver shares
#'
#' Calculates the shares (first choice or preference share) for a ValueDriver
#' like study.
#'
#'
#' @param design A matrix containing the design including the concepts to
#' simulate - in case it is not dummy coded use \code{dummy = FALSE} to apply
#' dummy coding via \code{\link{convertSSItoDesign}}
#' @param utils A matrix containing the utilities
#' @param nlev A vector indicating the number of levels per attribute
#' @param weight A vector with the weights (one per respondent)
#' @param FC A boolean variable indicating if first choice simulation should be
#' used (\code{FALSE} indicates preference share simulation) - default
#' \code{FALSE}
#' @param dummy A boolean variable indicating if the design file needs to be
#' dummy-coded using \code{\link{convertSSItoDesign}}. - default \code{TRUE}
#' @return A list including elements \item{meanShares}{aggregated shares
#' accross all respondents} \item{indShares}{individual shares for each
#' respondent}
#' @author Maximilian Rausch - Maximilian.Rausch@@tns-infratest.com
#' @examples
#'
#' utils_mat <- VMinR$VDdata$utils_mat
#' nlev <- VMinR$VDdata$nlev
#' weight <- VMinR$VDdata$weight
#' basecase <- as.matrix(VMinR$basecase_dummy_2)
#'
#' sim_BaseR <- VD.computeShares(design = basecase,
#'                               utils = utils_mat,
#'                               nlev = nlev,
#'                               weight = weight,
#'                               FC = FALSE)
#'
#' round(sim_BaseR$meanShares, 3)
#'
#' @export VD.computeShares
VD.computeShares <- function(
    design,
    utils,
    nlev,
    weight = NULL,
    FC = FALSE,
    dummy = TRUE) {
  if (!dummy) dummy_design <- as.matrix(convertSSItoDesign(design, nlev = nlev))
  if (dummy) dummy_design <- design

  if (is.null(weight)) weight <- rep(1, dim(utils)[1])

  xBeta <- t(dummy_design %*% t(utils))

  exp_xbeta <- exp(xBeta)

  if (FC) {
    FC_func <- function(x) {
      fc <- (x == max(x)) * 1
      fc_scaled <- fc / sum(fc)
      return(fc_scaled)
    }

    probabilities <- t(apply(xBeta, 1, FC_func))
  }
  else {
    probabilities <- exp_xbeta / rowSums(exp_xbeta)
  }

  return(list(meanShares = apply(probabilities, 2, stats::weighted.mean, w = weight),
              indShares = probabilities))
}
