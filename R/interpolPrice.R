# function computes the design matrix with interpolated price

#' Design matrix with interpolated price
#'
#' Include a interpolated brand specific price in a design matrix
#'
#'
#' @param scenario A matrix with the scenario for which the Strategy Profile should be calculated
#' @param priceSim the price to be simulated
#' @param prices A list containing the prices for the brand specific price attributes,
#'               e.g. the part from from \code{\link{VD.read_def}}
#' @param scenDUMMY A matrix with the dummy coded scenario
#' @param priceInd A vector with the index values for the brand specific price attributes
#' @param priceInd_Dummy A vector with the index values for the brand specific price attributes
#'                       in the dummy coded scenario
#' @param nlev A vector with the number of levels per attribute
#' @param scenInd An integer that indicates the scenario for which the Strategy Profile should be calculated
#' @param brandAtt An integer indicating the brand attribute in case of brand specific prices
#' @return A matrix with the dummy coded design matrix including the interpolation for the price.
#' @author Maximilian Rausch - Maximilian.Rausch@@bms-net.de
#' @examples
#'
#' \dontrun{
#' # Scenario
#' utils_mat <- VMinR$VDdata$utils_mat
#' defIN <- VD.read_def(file = "data/TEST_timtim_5seg_2_gew_gew2.def",
#'                      nlev = c(4, 6, 4, 2, 2, 2, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5))
#' nlev <- defIN$nlev
#'
#' scenario <- rbind(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
#'                   c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0),
#'                   c(1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0),
#'                   c(1, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0),
#'                   c(1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0),
#'                   c(1, 6, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0),
#'                   c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
#'
#' priceInd <- grep("^Price", names(defIN$att_List))
#'
#' prices <- lapply(defIN$att_List[priceInd],
#' function(p) {
#'   as.numeric(gsub("€ ", "", p))
#' })
#'
#' scenDUMMY <- convertSSItoDesign(scenario,
#'                                 nlev = c(nlev, 1))
#' priceInd_Dummy <- grep(paste0("ATT(", paste0(priceInd, collapse = "|"), ")"), names(scenDUMMY))
#'
#' scenINTER <- interpolPrice(priceSim = 300,
#'                            prices = prices,
#'                            scenDUMMY = scenDUMMY,
#'                            priceInd_Dummy = priceInd_Dummy,
#'                            nlev = nlev,
#'                            scenInd = 1,
#'                            brandAtt = 2)
#'
#' scenINTER
#' }
#'
#' @export StrategyProfile
interpolPrice <- function(
    scenario,
    priceSim,
    prices,
    scenDUMMY,
    priceInd,
    priceInd_Dummy,
    nlev,
    scenInd = 1,
    brandAtt = 1) {

  base_pr_lev <- rep(0, length(priceInd))

  brandInd <- scenario[scenInd, brandAtt]

  base_pr_lev[brandInd] <- stats::approx(prices[[brandInd]],
                                         seq_along(prices[[brandInd]]),
                                         xout = priceSim)$y

  designHelp <- t(as.matrix(c(brandInd,
                              diag(nlev[brandAtt])[brandInd, ] * base_pr_lev[brandInd])))


  base_design_close <- convertSSItoDesign(t(base_pr_lev),
                                          nlev = nlev[priceInd])

  row_interpol <- which(base_pr_lev[brandInd] %% 1 != 0)
  base_design <- base_design_close
  col_interpol <- which(base_design_close[1, ] != 0)
  base_design[, c(col_interpol, col_interpol + 1)] <- c(1 - (base_pr_lev[brandInd] %% 1),
                                                        (base_pr_lev[brandInd] %% 1))

  scenDUMMY_interpol <- scenDUMMY
  scenDUMMY_interpol[scenInd, priceInd_Dummy] <- base_design

  as.matrix(scenDUMMY_interpol)
}
