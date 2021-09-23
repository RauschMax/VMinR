# function computes shares for a BMS Pricer study - First Choice and Preference Share

#' Compute BMS Pricer shares
#'
#' Calculates the shares (first choice or preference share) for a BMS Pricer
#' like study.
#'
#'
#' @param utils A matrix containing the utilities
#' @param prices A matrix containing the prices used in the interview
#' @param simPrices A vector containing the prices to be used in the simulated scenario
#' @param simSKUs A vector containing indicies of the SKUs to be used in the simulated scenario
#' @param nlev An vector indicating the number of levels per attribute
#' @param weight A vector with the weights (one per respondent). Will be set to 1 if \code{NULL}
#' @param none A boolean variable indicating if NONE should be used in the simulated scenario. - default \code{FALSE}
#' @param iaw A matrix of individual awareness factors corresponding to those in the BMS Pricer tool.
#' All are set to 1 if \code{NULL}. In case both individual awareness and distribution factors need to be used
#' the respective matrices need to be multiplied before passing to this function.
#' @param FC A boolean variable indicating if first choice simulation should be used (\code{FALSE} indicates
#' preference share simulation) - default \code{TRUE}
#' @return A list including elements
#'   \item{ind_sim}{individual shares for each respondent}
#'   \item{Xbeta}{\code{X * beta} matrix used to calculate the shares (utility sums).}
#'   \item{simPrices}{respective input object passed through}
#'   \item{simSKUs}{respective input object passed through}
#'   \item{prices}{respective input object passed through}
#'   \item{iaw}{respective input object passed through}
#'   \item{none}{respective input object passed through}
#'   \item{weight}{respective input object passed through}
#'   \item{simShares}{aggregated shares accross all respondents}
#' @author Maximilian Rausch - Maximilian.Rausch@@bms-net.de
#' @examples
#'
#' \dontrun{
#' beer_def <- beer_data$def
#'
#' sim_Beer <- BMS.computeShares(beer_data$utils_mat,
#'                              beer_def$prices,
#'                              beer_def$prices[,3],
#'                              simSKUs = NULL,
#'                              nlev = beer_data$nlev,
#'                              weight = beer_data$weight,
#'                              none = FALSE,
#'                              iaw = NULL,
#'                              FC = FALSE)
#'
#' round(sim_Beer$simShares, 3)
#' }
#'
#' @export BMS.computeShares
BMS.computeShares <- function(utils, prices, simPrices, simSKUs = NULL, nlev,
                             weight = NULL, none = FALSE, iaw = NULL, FC = FALSE) {
  if (is.null(simSKUs)) stop("We need to know the SKUs to be simulated please!")

  if (is.null(weight)) {
    weight <- rep(1, dim(utils)[1])
    cat("No weight supplied. Weight is set to 1.\n")
  }

  if (!is.null(iaw)) {
    ind_iaw <- switch(none + 1, simSKUs, c(simSKUs, dim(prices)[1] + 1))
    iaw <- t(iaw[, ind_iaw])
  }

  price_max <- sapply(prices, max)
  price_min <- sapply(prices, min)

  failPrice <- FALSE
  if (any(price_max[simSKUs] < simPrices)) failPrice <- TRUE
  if (any(price_min[simSKUs] > simPrices)) failPrice <- TRUE

  if (failPrice) stop("\nCheck simPrices (index referring to simSKUs):\n",
                      "These Price(s) are larger than the MAX price of their SKU:\n",
                      paste(which(price_max[simSKUs] < simPrices), collapse = ", "),
                      ifelse(any(price_max[simSKUs] < simPrices), "", " -"),
                      "\n\nThese Price(s) are smaller than the MIN price of their SKU:\n",
                      paste(which(price_min[simSKUs] > simPrices), collapse = ", "),
                      ifelse(any(price_min[simSKUs] > simPrices), "", " -"))


  # price_ind <- matrix(seq(1, dim(prices)[1] * dim(prices)[2]) + dim(prices)[1],
  #                     nrow = dim(prices)[1], ncol = dim(prices)[2], byrow = TRUE)

  base_pr_lev <- rep(NA, length(prices))
  for (i in seq_along(simSKUs)) {
    base_pr_lev[simSKUs[i]] <- stats::approx(prices[[simSKUs[i]]],
                                             seq_along(prices[[simSKUs[i]]]),
                                             xout = simPrices[simSKUs[i]])$y
  }


  if (length(simSKUs) == 1) {
    designHelp <- t(as.matrix(c(simSKUs,
                                diag(nlev[1])[simSKUs, ] * base_pr_lev[simSKUs])))
  } else {
    designHelp <- cbind(simSKUs,
                        diag(nlev[1])[simSKUs, ] * base_pr_lev[simSKUs])
  }
  base_design_close <- as.matrix(cbind(convertSSItoDesign(designHelp,
                                                          nlev = nlev), 0))

  row_interpol <- which(base_pr_lev[simSKUs] %% 1 != 0)

  base_design <- base_design_close
  for (i in row_interpol) {
    col_interpol <- which(base_design_close[i, ] != 0)[2]
    base_design[i, c(col_interpol, col_interpol + 1)] <- c(1 - (base_pr_lev[simSKUs] %% 1)[i],
                                                           (base_pr_lev[simSKUs] %% 1)[i])
  }
  base_design[row_interpol, ]

  if (none) {
    # base_design <- cbind(base_design, 0)
    base_design <- rbind(base_design, c(rep(0, dim(base_design)[2] - 1), 1))
  }

  # Xbeta <- base_design %*% t(as.matrix(utils))
  # if (none){
  #   exp_xbeta <- exp(xbeta) * matrix(c(diag(design), 1), ncol=dim(xbeta)[2], nrow=dim(xbeta)[1], byrow = TRUE)
  # } else {
  #   exp_xbeta <- exp(xbeta) * matrix(diag(design), ncol=dim(xbeta)[2], nrow=dim(xbeta)[1], byrow = TRUE)
  # }

  Xbeta <- base_design %*% t(as.matrix(utils))
  exp_xbeta <- exp(Xbeta)

  # logit transformation o futility sums
  # sim <- exp_xbeta / matrix(rep(colSums(exp_xbeta), dim(Xbeta)[1]), ncol=dim(Xbeta)[2], byrow=TRUE)

  if (FC) {
    if (is.null(iaw)) iaw <- 1
    sim <- apply(Xbeta * iaw, 2, function(x) {(x == max(x))}) * 1
  } else {
    # Share of Preference
    sim <- exp_xbeta / matrix(rep(colSums(exp_xbeta), dim(Xbeta)[1]), ncol = dim(Xbeta)[2], byrow = TRUE)
    if (!is.null(iaw)) {
      sim <- sim * iaw
      sim <- sim / colSums(sim)
    }
  }

  invisible(list(ind_sim = sim,
                 Xbeta = Xbeta,
                 simPrices = simPrices,
                 simSKUs = simSKUs,
                 prices = prices,
                 iaw = iaw,
                 none = none,
                 weight = weight,
                 simShares = apply(as.matrix(sim), 1, stats::weighted.mean, w = weight)))
}
