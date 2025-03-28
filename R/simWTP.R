# function simulates interpolated prices for WTP calculation

#' Simulate scenario with interpolated price
#'
#' Simulate scenario with interpolated price to be able to calculate simulation based WTP
#'
#'
#' @param pr price level to be interpolated (needs to be with the range of prices)
#' @param prices A list containing the prices for the brand specific price attributes,
#'               e.g. the part from from \code{\link{VD.read_def}}
#' @param scenDUMMY_WTP A matrix with the dummy coded scenario
#' @param scenario_WTP A matrix with the scenario
#' @param priceInd A vector with the index values for the brand specific price attributes
#' @param priceInd_Dummy A vector with the index values for the brand specific price attributes
#'                       in the dummy coded scenario
#' @param nlev A vector with the number of levels per attribute
#' @param scenInd An integer that indicates the scenario for which the Strategy Profile should be calculated
#' @param brandAtt An integer indicating the brand attribute in case of brand specific prices.
#'                 \code{NULL} in case of no brand specific price.
#' @param utils A matrix containing the utilities per respondent of the model.
#' @param nlevIN A vector with the number of levels per attribute including the \code{NONE} if applicable
#' @param weight A vector with the weights (one per respondent)
#' @param FC A boolean variable indicating if first choice simulation should be
#'           used (\code{FALSE} indicates preference share simulation) - default \code{FALSE}
#' @return A numeric value returning the preference share for the indicated scenario
#' @author Maximilian Rausch - Maximilian.Rausch@@bms-net.de
#' @examples
#'
#' \dontrun{
#' simWTP(...)
#' }
#'
#' @export simWTP
simWTP <- function(pr,
                   prices = prices,
                   scenDUMMY_WTP,
                   scenario_WTP,
                   priceInd,
                   priceInd_Dummy,
                   nlev,
                   scenInd = 1,
                   brandAtt = 1,
                   utils,
                   nlevIN = c(nlev, 1),
                   weight = NULL,
                   FC = FALSE) {

  scenINTER <- interpolPrice(scenario = scenario_WTP,
                             priceSim = pr,
                             prices = prices,
                             scenDUMMY = as.data.frame(scenDUMMY_WTP),
                             priceInd = priceInd,
                             priceInd_Dummy = priceInd_Dummy,
                             nlev = nlevIN,
                             scenInd = scenInd,
                             brandAtt = brandAtt)

  Sim_ScenDummy <- VD.computeShares(design = as.matrix(scenINTER),
                                    utils = utils,
                                    nlev = c(nlevIN, 1),
                                    weight = weight,
                                    FC = FC,
                                    dummy = TRUE)

  Sim_ScenDummy$meanShares[scenInd]

}
