# function used as an input for optim to calculate simulation based WTP

#' Calculate simulation based WTP
#'
#' Function used as an input for optim to calculate simulation based WTP
#'
#'
#' @param pr price level to be interpolated (needs to be with the range of prices)
#' @param prices A list containing the prices for the brand specific price attributes,
#'               e.g. the part from from \code{\link{VD.read_def}}
#' @param baseSim A numeric value containing the preference share for the reference scenario for the WTP
#' @param scenWTP A matrix with the dummy coded scenario (\code{scenDUMMY_WTP})
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
#' @export optWTP
optWTP <- function(pr,
                   prices,
                   baseSim,
                   scenWTP,
                   priceInd,
                   priceInd_Dummy,
                   nlev,
                   scenInd = 1,
                   brandAtt,
                   utils,
                   nlevIN,
                   weight = NULL,
                   FC = FALSE) {
  abs(baseSim - VMinR::simWTP(pr = pr,
                              prices = prices,
                              scenDUMMY_WTP = scenWTP,
                              priceInd = priceInd,
                              priceInd_Dummy = priceInd_Dummy,
                              nlev = nlev,
                              scenInd = scenInd,
                              brandAtt = brandAtt,
                              utils = utils,
                              nlevIN = nlevIN,
                              weight = NULL,
                              FC = FALSE))
}
