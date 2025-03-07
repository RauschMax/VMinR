#' Compute profit for a ValuePricer study
#'
#' Calculates the profit for a ValuePricer like study.
#'
#'
#' @param utils A matrix containing the utilities
#' @param prices A matrix containing the prices used in the interview
#' @param simPrices A vector containing the prices to be used in the simulated
#' scenario
#' @param simSKUs A vector containing indicies of the SKUs to be used in the
#' simulated scenario
#' @param nlev An vector indicating the number of levels per attribute
#' @param weight A vector with the weights (one per respondent). Will be set to
#' 1 if \code{NULL}
#' @param none A boolean variable indicating if NONE should be used in the
#' simulated scenario. - default \code{FALSE}
#' @param iaw A matrix of individual awareness factors corresponding to those
#' in the ValuePricer tool. All are set to 1 if \code{NULL}. In case both
#' individual awareness and distribution factors need to be used the respective
#' matrices need to be multiplied before passing to this function.
#' @param FC A boolean variable indicating if first choice simulation should be
#' used (\code{FALSE} indicates preference share simulation) - default
#' \code{TRUE}
#' @param costs A vector indicating the costs per SKU in the model.
#' @return A list including elements \item{profit}{mean profit across all respondents}
#' \item{demand}{mean share of the scenario across all respondents}
#' @author Maximilian Rausch - Maximilian.Rausch@@kantartns.com
#' @examples
#'
#' beer_def <- beer_data$def
#'
#' profit_Beer <- VP.computeProfit(beer_data$utils_mat,
#'                                 beer_def$prices,
#'                                 beer_def$prices[,3],
#'                                 simSKUs = NULL,
#'                                 nlev = beer_data$nlev,
#'                                 weight = beer_data$weight,
#'                                 none = FALSE,
#'                                 iaw = NULL,
#'                                 FC = FALSE,
#'                                 costs = 0.6 * beer_def$prices[,3])
#'
#' profit_Beer$profit
#'
#' @export VP.computeProfit

VP.computeProfit <- function(utils, prices, simPrices, simSKUs = NULL, nlev, weight = NULL, none = FALSE,
                             iaw = NULL, FC = FALSE, costs) {
  if (is.null(simSKUs)) ifelse(length(simPrices) == dim(prices)[1],
                              simSKUs <- seq_along(simPrices), stop("We need to know the SKUs to be simulated please!"))

  if (is.null(weight)) weight <- rep(1, dim(utils)[1])

  ind_SKUnone <- switch(none + 1, simSKUs, c(simSKUs, dim(prices)[1] + 1))
  if (!is.null(iaw)) {
    iaw <- t(iaw[, ind_SKUnone])
  }

  simPrices <- diag(simPrices)
  costs <- diag(costs)

  #new: added to interpolate prices for a part worth design - rm 151125
  price_ind <- matrix(seq(1, dim(prices)[1] * dim(prices)[2]) + length(diag(simPrices)),
                      nrow = dim(prices)[1], ncol = dim(prices)[2], byrow = TRUE)

  base_pr_lev <- rep(NA, length(diag(simPrices)))
  for (i in seq_along(diag(simPrices))) {
    base_pr_lev[i] <- stats::approx(prices[i, ], sequence(dim(prices)[2]), xout = diag(simPrices)[i])$y
  }

  base_design_close <- as.matrix(cbind(VMinR::convertSSItoDesign(cbind(simSKUs,
                                                                       diag(nlev[1])[simSKUs, ] * base_pr_lev[simSKUs]),
                                                                 nlev = nlev), 0))

  row_interpol <- which(base_pr_lev[simSKUs] %% 1 != 0)

  base_design <- base_design_close
  for (i in row_interpol) {
    col_interpol <- which(base_design_close[i, ] != 0)[2]
    base_design[i, c(col_interpol, col_interpol + 1)] <- c(1 - (base_pr_lev[simSKUs] %% 1)[i],
                                                           (base_pr_lev[simSKUs] %% 1)[i])
  }

  if (none) {
    # base_design <- cbind(base_design, 0)
    base_design <- rbind(base_design, c(rep(0, dim(base_design)[2] - 1), 1))

    simPrices <- cbind(simPrices, 0)
    simPrices <- rbind(simPrices, c(rep(0, dim(simPrices)[1]), 0))

    costs <- cbind(costs, 0)
    costs <- rbind(costs, c(rep(0, dim(costs)[1]), 0))
  }

  xbeta <- base_design %*% t(as.matrix(utils))
  exp_xbeta <- exp(xbeta)

  #end new 151125
  # First Choice
  if (FC) {
    if (is.null(iaw)) iaw <- 1
    probabilities <- apply(xbeta * iaw, 2, function(x) {(x == max(x))}) * 1
  }
  else {
    # Share of Preference
    probabilities <- exp_xbeta / matrix(rep(colSums(exp_xbeta), dim(xbeta)[1]), ncol = dim(xbeta)[2], byrow = TRUE)
    if (!is.null(iaw)) {
      probabilities <- probabilities * iaw
      probabilities <- probabilities / colSums(probabilities)
    }
  }

  profits <- apply(as.matrix(probabilities), 1,
                   stats::weighted.mean, w = weight)  * colSums(simPrices - costs)[ind_SKUnone]

  demand <- apply(as.matrix(probabilities), 1,
                  stats::weighted.mean, w = weight)

  list(profits = profits, demand = demand)
}
