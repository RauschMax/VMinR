#' Compute optimal prices for a ValuePricer study
#'
#' Calculates the optimal prices (based on the theory of John Nash) for a ValuePricer like study.
#'
#'
#' @param utils A matrix containing the utilities
#' @param simSKUs A vector containing indicies of the SKUs to be used in the simulated scenario
#' @param startingPrices A vector containing the prices to be used as starting point for the optimisation
#' @param prices A matrix containing the prices used in the interview
#' @param pr_range_mat A matrix (2 columns) containing the upper and lower boundaries for the optimisation.
#' @param nlev A vector indicating the number of levels per attribute
#' @param costs A vector indicating the costs per SKU in the model.
#' @param grp A vector indicating which SKUs should be considered as a group
#' (e.g. from the same brand) during optimisation. Same index indicates a group.
#' 1st column: sequencial index.
#' 2nd column: indicator for groups.
#' 3rd column: indicator for line pricing
#' @param line_price A boolean variable telling if line pricing should be applied
#' @param freeze A vector inculding the SKU numbers that should be excluded from the optimisation.
#' default = \code{NULL}
#' @param FC A boolean variable indicating if first choice simulation should be used (\code{FALSE} indicates preference
#' share simulation) - default \code{FALSE}
#' @param none A boolean variable indicating if NONE should be used in the simulated scenario.
#' default \code{FALSE}
#' @param weight A vector with the weights (one per respondent). Will be set to 1 if \code{NULL}
#' @param iaw A matrix of individual awareness factors corresponding to those in the ValuePricer tool.
#' All are set to 1 if \code{NULL}. In case both individual awareness and distribution factors need to be used
#' the respective matrices need to be multiplied before passing to this function.
#' @param constraints A vector indicating the allowed increase/decrease in share, profit or revenue
#' (in percentage points). If \code{NULL} (default) no constraints will be applied.
#' @param optKPI A string indicating the KPI to be optimized (profit, revenue, share). default \code{"profit"}.
#' @param shiny A boolean variable stating if the function is used in the shiny app. Adds progress bar parts.
#' default \code{TRUE}.
#' @return A vector of optimal prices given the specs.
#' @author Maximilian Rausch - m.rausch@@bms-net.de
#' @examples
#'
#' \dontrun{
#' beer_def <- beer_data$def
#' design_beer <- diag(length(beer_def$brands))
#'
#' input_list_Nash <- list(utils = beer_data$utils_mat[, -151],
#'                         simSKUs = seq_along(length(beer_def$brands)),
#'                         startingPrices = beer_def$prices[,3],
#'                         prices = beer_data$pricemat_tested,
#'                         pr_range_mat = NULL,
#'                         nlev = beer_data$nlev,
#'                         costs = beer_data$pricemat_tested[,1] * 0.8,
#'                         grp = NULL,
#'                         line_price = FALSE,
#'                         freeze = NULL,
#'                         FC = FALSE,
#'                         none = FALSE,
#'                         weight = beer_data$weight,
#'                         iaw = NULL,
#'                         constraints = NULL,
#'                         optKPI = "profit")
#'
#' # Nash optimization
#' Nash_run <- do.call(nashEquilibrium, input_list_Nash)
#' }
#'
#' @export nashEquilibrium

nashEquilibrium <- function(utils, simSKUs, startingPrices, prices, pr_range_mat = NULL, nlev, costs,
                            grp, line_price, freeze = NULL, FC = FALSE, none = FALSE,
                            weight = NULL, iaw = NULL, constraints = NULL, optKPI = "profit", shiny = FALSE) {

  prices_used <- startingPrices
  # costs <- diag(costs)
  if (is.null(pr_range_mat)) pr_range_mat <-  t(sapply(prices, range))
  if (is.null(grp)) grp <- cbind(sequence(nlev[1]), sequence(nlev[1]))       # added grouping list  rm 150227
  # grp <- grp[simSKUs, ]

  if (!is.null(constraints) & none & length(constraints) != nlev[1] + 1)  constraints <- c(constraints, -1)

  numIters <- 0
  valid <- FALSE
  # Convergence parameters
  tol <- .01
  maxIter <- 20
  base_profit <- BMS.computeProfit(utils, prices, simPrices = startingPrices, simSKUs,
                                  nlev, weight, none, iaw, FC, costs)

  ind_SKUnone <- switch(none + 1, simSKUs, c(simSKUs, dim(prices)[1] + 1))
  split_factor <- switch(none + 1, grp[simSKUs, 2], c(grp[simSKUs, 2], max(grp[, 2]) + 1))
  base_share  <- unlist(lapply(split(base_profit$demand, split_factor), sum))
  base_profit <- unlist(lapply(split(base_profit$profits / sum(base_profit$profits), split_factor), sum))
  if (!is.null(constraints)) const_grp <- unlist(lapply(split(constraints[ind_SKUnone], split_factor), mean))


  # adding parts for shiny progress bar !----
  if (shiny) {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    progress$set(message = "Running optimization", value = 0)
  }
  # !----


  if (optKPI == "profit") {
    while (!valid) {
     numIters <- numIters + 1
      oldPrices <- prices_used
        if (!line_price) {
        # w/o linepricing
        for (i in simSKUs) {
          test <- (i == freeze)
          if (any(test)) {
            prices_used[i] <- oldPrices[i]
          } else {
            cp <- function(p) {
              prices_used[i] <- p
              sum(BMS.computeProfit(utils, prices, simPrices = prices_used, simSKUs,
                                   nlev, weight, none, iaw, FC, costs)$profits[which(grp[simSKUs, 2] == grp[i, 2])])
              # added grouping list  rm 150227
            }
            oldPrices_const <- prices_used
            prices_used[i] <- stats::optimize(cp, interval = pr_range_mat[i, ], maximum = TRUE)$maximum
            if (!is.null(constraints)) {
              # Constraints: added 19.10.16 (rm)
              check_share <- unlist(lapply(split(BMS.computeShares(utils, prices, prices_used,
                                                                         simSKUs, nlev, weight,
                                                                         none, iaw, FC)$simShares,
                                                 split_factor), sum))
              test_const <- check_share - base_share
              if (any(test_const < const_grp)) {
                # pr_range_mat[i,2] <- max(pr_range_mat[i,1]+0.01, prices_used[i]-0.01)
                # Set maximum of price range to "smaller than optimized price"
                # --> not necessary
                prices_used <- oldPrices_const
              }
            }
          }}
      } else {
        # linepricing   added rm 150311
        for (i in simSKUs) {
          test <- (i == freeze)
          if (any(test)) {
            prices_used[i] <- oldPrices[i]
          } else {
            cp <- function(p) {
              prices_used[which(grp[, 3] == grp[i, 3] & prices_used != 0)] <- p
              sum(BMS.computeProfit(utils, prices, simPrices = prices_used, simSKUs,
                                   nlev, weight, none, iaw, FC, costs)$profits[which(grp[simSKUs, 2] == grp[i, 2])])
              # added grouping list  rm 150227
            }
            oldPrices_const <- prices_used
            prices_used[which(grp[, 3] == grp[i, 3] & prices_used != 0)] <- stats::optimize(cp,
                              interval = c(min(pr_range_mat[which(grp[, 3] == grp[i, 3]), 1]),
                                         max(pr_range_mat[which(grp[, 3] == grp[i, 3]), 2])), maximum = TRUE)$maximum
            if (!is.null(constraints)) {
              # Constraints: added 19.10.16 (rm)
              check_share <- unlist(lapply(split(BMS.computeShares(utils, prices, prices_used,
                                                                         simSKUs, nlev, weight, none,
                                                                         iaw, FC)$simShares,
                                                 split_factor), sum))
              test_const <- check_share - base_share
              if (any(test_const < const_grp)) {
                # pr_range_mat[i,2] <- max(pr_range_mat[i,1]+0.01, prices_used[i]-0.01)
                # Set maximum of price range to "smaller than optimized price"
                # --> not necessary
                prices_used <- oldPrices_const
              }
            }
          }}
      }

      if (shiny) {
        # Increment the progress bar
        progress$inc(1 / maxIter)

      }

      if (sum(abs(prices_used - oldPrices)) < tol || numIters == maxIter) {
        valid <- TRUE
      }
    }
  }
  else if (optKPI == "share") {
    while (!valid) {
      numIters <- numIters + 1
      oldPrices <- prices_used
      if (!line_price) {
        # w/o linepricing
        for (i in simSKUs) {
          test <- (i == freeze)
          if (any(test)) {
            prices_used[i] <- oldPrices[i]
          } else {
            cs <- function(p) {
              prices_used[i] <- p
              sum(BMS.computeShares(utils, prices, simPrices = prices_used, simSKUs,
                                          nlev, weight, none, iaw, FC)$simShares[which(grp[simSKUs, 2] == grp[i, 2])])
              # added grouping list  rm 150227
            }
            oldPrices_const <- prices_used
            prices_used[i] <- stats::optimize(cs, interval = pr_range_mat[i, ], maximum = TRUE)$maximum
            if (!is.null(constraints)) {
              # Constraints: added 19.10.16 (rm)
              check_Profit <- unlist(lapply(split(BMS.computeProfit(utils, prices, prices_used,
                                                                   simSKUs, nlev, weight, none, iaw, FC,
                                                                   costs)$profit /
                                                    sum(BMS.computeProfit(utils, prices, prices_used,
                                                                         simSKUs, nlev, weight, none,
                                                                         iaw, FC, costs)$profit),
                                                  split_factor), sum))

              test_const <- check_Profit - base_profit
              if (any(test_const < const_grp)) {
                # pr_range_mat[i,2] <- max(pr_range_mat[i,1]+0.01, prices_used[i]-0.01)
                # Set maximum of price range to "smaller than optimized price"
                # --> not necessary
                prices_used <- oldPrices_const
              }
            }
          }}
      } else {
        # linepricing   added rm 150311
        for (i in simSKUs) {
          test <- (i == freeze)
          if (any(test)) {
            prices_used[i] <- oldPrices[i]
          } else {
            cs <- function(p) {
              prices_used[which(grp[, 3] == grp[i, 3] & prices_used != 0)] <- p
              sum(BMS.computeShares(utils, prices, simPrices = prices_used, simSKUs,
                                          nlev, weight, none, iaw, FC)$simShares[which(grp[simSKUs, 2] == grp[i, 2])])
              # added grouping list  rm 150227
            }
            oldPrices_const <- prices_used
            prices_used[which(grp[, 3] == grp[i, 3] & prices_used != 0)] <- stats::optimize(cs,
                                      interval = c(min(pr_range_mat[which(grp[, 3] == grp[i, 3]), 1]),
                                            max(pr_range_mat[which(grp[, 3] == grp[i, 3]), 2])), maximum = TRUE)$maximum
            if (!is.null(constraints)) {
              # Constraints: added 19.10.16 (rm)
              check_Profit <- unlist(lapply(split(BMS.computeProfit(utils, prices, prices_used, simSKUs,
                                                                   nlev, weight, none, iaw, FC, costs)$profit /
                                                    sum(BMS.computeProfit(utils, prices, prices_used,
                                                                         simSKUs, nlev, weight, none, iaw,
                                                                         FC, costs)$profit),
                                                  split_factor), sum))

              test_const <- check_Profit - base_profit
              if (any(test_const < const_grp)) {
                # pr_range_mat[i,2] <- max(pr_range_mat[i,1]+0.01, prices_used[i]-0.01)
                # Set maximum of price range to "smaller than optimized price"
                # --> not necessary
                prices_used <- oldPrices_const
              }
            }
          }}
      }

      if (shiny) {
        # Increment the progress bar
        progress$inc(1 / maxIter)

      }

      if (sum(abs(prices_used - oldPrices)) < tol || numIters == maxIter) {
        valid <- TRUE
      }
    }
  } else {
    stop("You need to specifiy a KPI to be optimized (share/profit)")
  }
  return(prices_used)     # matrix
}
