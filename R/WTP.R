# function to calculate simulation based WTP

#' Simulation based WTP
#'
#' Function used to calculate simulation based WTP for all attribute levels of a scenario
#'
#'
#' @param scen A matrix with the scenario
#' @param scenIndex An integer that indicates the concept for which the WTP should be calculated
#' @param nLevels A vector with the number of levels per attribute
#' @param attNames A list with attribute and level labels (e.g. \code{att_list} from \code{\link{VD.read_def}})
#' @param brandAttribute An integer indicating the brand attribute in case of brand specific prices.
#'                       \code{NULL} in case of no brand specific price.
#' @param priceIndex A vector with the index values for the brand specific price attributes
#' @param priceDefault A vector with the default levels to be used for prices (in case of asd)
#' @param prices A list containing the prices for the brand specific price attributes,
#'               e.g. the part from from \code{\link{VD.read_def}}
#' @param utilsIN A matrix containing the utilities per respondent of the model.
#' @param none A boolean variable indicating if NONE should be used in the simulated scenario. - default \code{TRUE}
#' @param weight A vector with the weights (one per respondent)
#' @param FC A boolean variable indicating if first choice simulation should be
#'           used (\code{FALSE} indicates preference share simulation) - default \code{FALSE}
#' @return A list including elements
#'   \item{WTP_List}{A list containing the WTP values per attribute and level}
#'   \item{WTP_Scen}{A list containing the scenarios used for WTP calculation}
#'   \item{ref_Scen}{A list with the simulation results for the reference scenario (\code{\link{VD.computeShares}})}
#'   \item{WTP_DT}{A data.table with the WTP values for each level (excl. price attributes)}
#' @author Maximilian Rausch - Maximilian.Rausch@@bms-net.de
#' @examples
#'
#' \dontrun{
#' WTP(...)
#' }
#'
#' @export WTP
#' @import data.table
WTP <- function(
    scen,
    scenIndex,
    nLevels,
    attNames,
    brandAttribute,
    priceIndex,
    priceDefault,
    prices,
    utilsIN,
    none = TRUE,
    weight = NULL,
    FC = FALSE) {

  if (none) {
    nlev_none <- c(nLevels, 1)
  } else {
    nlev_none <- nLevels
  }

  scenDUMMY <- VMinR::convertSSItoDesign(scen,
                                         nlev = nlev_none)

  Sim_Scen <- VMinR::VD.computeShares(design = as.matrix(scenDUMMY),
                                      utils = utilsIN,
                                      nlev = nlev_none,
                                      weight = weight,
                                      FC = FC,
                                      dummy = TRUE)

  priceInd_Dummy <- grep(paste0("ATT(", paste0(priceIndex, collapse = "|"), ")-"),
                         names(scenDUMMY))

  WTP_Help <- lapply(which(scen[scenIndex, ] != 0),
                     function(i) {
                       # i <- 2
                       scenList <- lapply(sequence(nLevels[i]),
                                          function(j) {
                                            # j <- 1
                                            out <- scen

                                            pHelp <- out[scenIndex,
                                                         priceIndex]

                                            pLevHelp <- pHelp[pHelp != 0]

                                            out[scenIndex, i] <- j

                                            if (!is.null(brandAttribute)) {
                                              if (i == brandAttribute) {
                                                out[scenIndex,
                                                    priceIndex[which(pHelp > 0)]] <- 0
                                                out[scenIndex,
                                                    priceIndex[j]] <- ifelse(which(pHelp > 0) == j,
                                                                             pLevHelp,
                                                                             priceDefault[j])

                                              }
                                            }
                                            out
                                          })
                       names(scenList) <- attNames[[i]]
                       scenList
                     })

  names(WTP_Help) <- names(attNames)[which(scen[scenIndex, ] != 0)]

  WTP_List <- lapply(
    seq_along(WTP_Help),
    function(l1) {
      # l1 <- 1
      wtp_att <- sapply(
        seq_along(WTP_Help[[l1]]),
        function(l2) {
          # l1 <- 1
          # l2 <- 2
          scenHelp <- WTP_Help[[l1]][[l2]]

          priceInterval_help <- which(scenHelp[scenInd, priceInd] != 0)

          opt <- stats::optimize(VMinR::optWTP,
                                 prices = prices,
                                 baseSim = Sim_Scen$meanShares[scenIndex],
                                 scenWTP_DUMMY = as.matrix(convertSSItoDesign(scenHelp,
                                                                              nlev = nlev_none)),
                                 scenWTP = scenHelp,
                                 priceInd = priceIndex,
                                 priceInd_Dummy = priceInd_Dummy,
                                 nlev = nLevels,
                                 scenInd = scenIndex,
                                 brandAtt = brandAttribute,
                                 utils = utilsIN,
                                 nlevIN = nlev_none,
                                 weight = weight,
                                 FC = FC,
                                 interval = range(prices[priceInterval_help]))

          opt$minimum
        })

      data.table::data.table(Att = names(WTP_Help)[l1],
                             Level = names(WTP_Help[[l1]]),
                             WTP = wtp_att)
    })

  priceBase_WTP <- sum(VMinR::convertSSItoDesign(scen,
                                                 nlev = nlev_none)[scenIndex, priceInd_Dummy] * unlist(prices))

  WTP_DT <- data.table::rbindlist(WTP_List)
  WTP_DT[AttInd %in% c(priceIndex, brandAttribute), WTP := NA]
  WTP_DT[, delta := round(WTP - priceBase_WTP, 2)]
  WTP_DT[, WTP := round(WTP, 2)]
  WTP_DT[!(AttInd %in% c(priceIndex, brandAttribute)),
         exceed := (WTP %in% range(prices[[scenInd]])) * sign(delta)]
  WTP_DT

  invisible(list(WTP_List = WTP_List,
                 WTP_Scen = WTP_Help,
                 ref_Scen = Sim_Scen,
                 WTP_DT = WTP_DT))
}
