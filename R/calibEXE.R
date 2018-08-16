#' Calib EXE
#'
#' Calibrates the utilities based on purchase intention questions (calibEXE)
#'
#'
#' @param BWconcepts A matrix/data.frame containing the best and worst concepts per respondents.
#' @param PI A matrix containing the answer to the purchase intention questions
#' for the best/worst concepts.
#' @param utils A matrix containing the utilities per respondent of the model.
#' @param cut An integer value indicating the maximal value for utilities after calibration.
#' @param nlev A vector indicating the number of levels per attribute
#' @param PIsteps A vector with the purchse intention %-value to be used for the calibration
#' (default: \code{c(.95, .5, .3, .15, .05)})
#' @return A list including 13 elements
#' \item{BWconcepts}{A matrix/data.frame containing the best and worst concepts per respondents
#' which have been passed to the function}
#' \item{utils_calib}{A matrix containing the CALIBRATED utilities per respondent of the model.}
#' \item{utils}{A matrix containing the original utilities per respondent of the model.}
#' \item{check_order_BW}{A vector containing boolean values indicating if the utilities
#' for the best concept have been greater than the ones of the worst concept.}
#' \item{check_order_PI}{A vector containing boolean values indicating if the purchase intentions
#' for the best concept have been greater than the ones of the worst concept.}
#' \item{utl_sum}{A matrix containing the corrected utility sums for the best and worst concept.
#' Values in the wrong order are set to be equal.}
#' \item{PurchaseInt}{A matrix containing the corrected purchase intentions for the best
#' and worst concept. (1 = .95, 2 = .5, 3 = .3, 4 = .15, 5 = .05)
#' Values in the wrong order are set to be equal.}
#' \item{utl_sum_ORIG}{A matrix containing the original (uncorrected) utility sums for
#' the best and worst concept.}
#' \item{PurchaseInt_ORIG}{A matrix containing the original (uncorrected) purchase intentions for the best
#' and worst concept.}
#' \item{a}{The slope of the linear function used for the calibration.}
#' \item{b}{The intercept of the linear function used for the calibration.}
#' \item{nlev}{A vector indicating the number of levels per attribute}
#' \item{cut}{An integer indicating the cutoff value used for the maximal
#' value for utilities after calibration.}
#' @author Maximilian Rausch - Maximilian.Rausch@tns-infratest.com
#' @examples
#'
#' \dontrun{
#' calibData <- calibEXE(BWconcepts = BWconcepts[, c(paste0("B_Att_", sequence(natt)),
#'                                                   paste0("W_Att_", sequence(natt)))],
#' PI = BWconcepts[, c("PI_B", "PI_W")],
#' utils = dat_input$utils_mat,
#' cut = 42, nlev = nlev)
#' }
#'
#' @export calibEXE



calibEXE <- function(BWconcepts = NULL, PI = NULL, utils = NULL, cut = 42, nlev = NULL,
                     PIsteps = c(.95, .5, .3, .15, .05)) {
  # some checks ----
  if (is.null(BWconcepts)) stop("Please provide best and worst concepts!")
  if (is.null(PI)) stop("Please provide purchase intention for best and worst concepts!")
  if (is.null(utils)) stop("Please provide utilities!")
  if (is.null(nlev)) stop("Please provide vector of number of levels!")

  if (nrow(BWconcepts) != nrow(PI)) stop("BWconcepts and PI need to have the same number of cases/rows")
  if (nrow(BWconcepts) != nrow(utils)) stop("BWconcepts and utilities need to have the same number of cases/rows")
  if (nrow(PI) != nrow(utils)) stop("PI and utilities need to have the same number of cases/rows")

  natt <- length(nlev)

  # utility sums best/worst concepts ----
  best_dummy <- as.matrix(cbind(convertSSItoDesign(BWconcepts[, 1:(ncol(BWconcepts) / 2)], nlev = nlev), 0))
  worst_dummy <- as.matrix(cbind(convertSSItoDesign(BWconcepts[, (1 + ncol(BWconcepts) / 2):ncol(BWconcepts)],
                                                    nlev = nlev), 0))

  # utl sums - X%*%beta ----
  utl_sum_best <- rowSums(utils * best_dummy)
  utl_sum_worst <- rowSums(utils * worst_dummy)

  utl_sum <- round(cbind(utl_sum_best, utl_sum_worst), 3)
  colnames(utl_sum) <- c("best", "worst")

  # check if best is better than worst
  # Set utl_sum_worst == utl_sum_best if WRONG ORDER
  check_order <- apply(utl_sum, 1, function(x) {x[1] > x[2]})
  utl_sum_ORIG <- utl_sum
  utl_sum[!check_order, 2] <- utl_sum[!check_order, 1] - 0.001

  # rescale 5 point scale to %-values ----
  # DEFAULT
  # 1 --> 0.95  definitly would buy
  # 2 --> 0.50
  # 3 --> 0.30
  # 4 --> 0.15
  # 5 --> 0.05  definitely would not buy

  PurchaseInt_best <- unlist(lapply(as.integer(PI[, 1]), switch,
                                    PIsteps[1], PIsteps[2], PIsteps[3], PIsteps[4], PIsteps[5]))
  PurchaseInt_worst <- unlist(lapply(as.integer(PI[, 2]), switch,
                                     PIsteps[1], PIsteps[2], PIsteps[3], PIsteps[4], PIsteps[5]))

  check_recode <- cbind(PI[, 1], PurchaseInt_best,
                        PI[, 2], PurchaseInt_worst)
  PurchaseInt <- check_recode[, c(2, 4)]
  colnames(PurchaseInt) <- c("best", "worst")

  # check if best is rated better than worst
  # Set PI_worst == PI_best if WRONG ORDER
  check_order_PI <- apply(PurchaseInt, 1, function(x) {x[1] > x[2]})
  PurchaseInt_ORIG <- PurchaseInt
  PurchaseInt[!check_order_PI, 2] <- PurchaseInt[!check_order_PI, 1]

  # logit transformation ----

  logit_PurchaseInt <- log(PurchaseInt / (1 - PurchaseInt))

  lm_data <- data.frame(logPI = as.vector(t(logit_PurchaseInt)),
                        x = as.vector(t(utl_sum)))

  lm_coeff <- matrix(NA, ncol = 2, nrow = nrow(logit_PurchaseInt))
  count <- 0
  for (i in seq(1, 2 * nrow(lm_coeff), by = 2)) {
    count <- count + 1
    lm_coeff[count, ] <- stats::lm(logPI ~ 1 + x, data = lm_data[i:(i + 1), ])$coeff
  }

  a <- lm_coeff[, 1]
  b <- lm_coeff[, 2]

  # calibrated utilities - calib.exe ----
  utils_calib <- utils * b + a / natt

  # set values > abs(42) to 42
  if (!is.null(cut)) {
    utils_calib[which(abs(utils_calib) > 42, arr.ind = TRUE)] <-
      42 * sign(utils_calib[which(abs(utils_calib) > 42, arr.ind = TRUE)])
  }

  invisible(list(BWconcepts = BWconcepts,
                 utils_calib = utils_calib,
                 utils = utils,
                 check_order_BW = check_order,
                 check_order_PI = check_order_PI,
                 utl_sum = utl_sum,
                 PurchaseInt = PurchaseInt,
                 logit_PurchaseInt = logit_PurchaseInt,
                 lm_data = lm_data,
                 utl_sum_ORIG = utl_sum_ORIG,
                 PurchaseInt_ORIG = PurchaseInt_ORIG,
                 a = a,
                 b = b,
                 nlev = nlev,
                 cut = cut,
                 PIsteps = PIsteps))

}
