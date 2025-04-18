#' Summary of calibEXE
#'
#' Runs a summary for the calibration of utilities using the calibEXE function.
#'
#'
#' @param calibData A list containing the results of \code{\link{calibEXE}}
#' @param outfile A text value indicating the label of the output file.
#' Default \code{"summary.txt"}.
#' @param infile A text value indicating the label input file for
#' \code{\link{calibEXE}} function. Default \code{"INFILE"}.
#' @return No output generated. Summary will be exported to the indicated file.
#' @author Maximilian Rausch - Maximilian.Rausch@@tns-infratest.com
#' @examples
#'
#' \dontrun{
#' summary_calibEXE(calibData, "summary.txt", infile = dat_input$def$file_in)
#' }
#'
#' @export summary_calibEXE

summary_calibEXE <- function(calibData = calibData, outfile = "summary.txt", infile = "INFILE") {
  # calculate Product Acceptance for best and worst concepts and compare to Purchase Intention question
  best_dummy <- as.matrix(convertSSItoDesign(calibData$BWconcepts[, 1:(ncol(calibData$BWconcepts) / 2)],
                                             nlev = calibData$nlev))
  worst_dummy <- as.matrix(convertSSItoDesign(
    calibData$BWconcepts[, (1 + ncol(calibData$BWconcepts) / 2):ncol(calibData$BWconcepts)],
    nlev = calibData$nlev))

  ProdAcc_best <- exp(rowSums(best_dummy * calibData$utils[, -ncol(calibData$utils)])) /
    (1 + exp(rowSums(best_dummy * calibData$utils[, -ncol(calibData$utils)])))

  ProdAcc_best_calib <- exp(rowSums(best_dummy * calibData$utils_calib[, -ncol(calibData$utils_calib)])) /
    (1 + exp(rowSums(best_dummy * calibData$utils_calib[, -ncol(calibData$utils_calib)])))

  ProdAcc_worst <- exp(rowSums(worst_dummy * calibData$utils[, -ncol(calibData$utils)])) /
    (1 + exp(rowSums(worst_dummy * calibData$utils[, -ncol(calibData$utils)])))

  ProdAcc_worst_calib <- exp(rowSums(worst_dummy * calibData$utils_calib[, -ncol(calibData$utils_calib)])) /
    (1 + exp(rowSums(worst_dummy * calibData$utils_calib[, -ncol(calibData$utils_calib)])))

  cat("SUMMARY \n", file = outfile)
  cat(infile, "\n\n", file = outfile, append = TRUE)

  order1 <- table(factor(calibData$check_order_BW, levels = c("FALSE", "TRUE")))

  cat("Utility sum in wrong order (worst > best): \n", file = outfile, append = TRUE)
  if (prop.table(order1)[1] > .10) {
    cat(" !!! WATCH OUT! More than 10% in wrong order !!! \n", file = outfile, append = TRUE)
  }
  cat("WRONG order: ", order1[1], "\n", file = outfile, append = TRUE)
  cat("RIGHT order: ", order1[2], "\n\n", file = outfile, append = TRUE)

  cat("Purchase intention in wrong order (worst >= best): \n", file = outfile, append = TRUE)
  # if (prop.table(table(calibData$check_order_PI))[1] > .10) {
  #   cat(" !!! WATCH OUT! More than 10% in wrong order !!! \n", file = outfile, append = TRUE)
  # }
  order2 <- table(factor(calibData$check_order_PI, levels = c("FALSE", "TRUE")))

  cat("WRONG order: ", order2[1], "\n", file = outfile, append = TRUE)
  cat("RIGHT order: ", order2[2], "\n\n", file = outfile, append = TRUE)

  order3 <- table(factor(!(calibData$PurchaseInt_ORIG[, 1] < calibData$PurchaseInt_ORIG[, 2]),
                  levels = c("FALSE", "TRUE")))

  cat("Purchase intention in wrong order (worst > best): \n", file = outfile, append = TRUE)
  if (prop.table(order3)[1] > .10) {
    cat(" !!! WATCH OUT! More than 10% in wrong order !!! \n", file = outfile, append = TRUE)
  }
  cat("WRONG order: ", order3[1],
      "\n", file = outfile, append = TRUE)
  cat("RIGHT order: ", order3[2],
      "\n\n", file = outfile, append = TRUE)

  cat("See csv outputs for details.\n", file = outfile, append = TRUE)
  cat("----------------------------------------------------------------------\n\n",
      file = outfile, append = TRUE)
  cat("Purchase Intention - average PI, direct questions:\n",
      file = outfile, append = TRUE)
  cat("BEST:   ", round(colMeans(calibData$PurchaseInt)[1], 2), "\n",
      file = outfile, append = TRUE)
  cat("WORST:  ", round(colMeans(calibData$PurchaseInt)[2], 2), "\n",
      file = outfile, append = TRUE)
  cat("----------------------------------------------------------------------\n\n",
      file = outfile, append = TRUE)
  cat("CORRELATIONS - calculated product acceptance vs. purchase intention question:\n",
      file = outfile, append = TRUE)
  cat("BEST (calibrated):   ", round(stats::cor(ProdAcc_best_calib, calibData$PurchaseInt[, 1]), 2), "\n",
      file = outfile, append = TRUE)
  cat("BEST (uncalibrated): ", round(stats::cor(ProdAcc_best, calibData$PurchaseInt[, 1]), 2), "\n",
      file = outfile, append = TRUE)
  cat("WORST (calibrated):  ", round(stats::cor(ProdAcc_worst_calib, calibData$PurchaseInt[, 2]), 2), "\n",
      file = outfile, append = TRUE)
  cat("WORST (uncalibrated):", round(stats::cor(ProdAcc_worst, calibData$PurchaseInt[, 2]), 2), "\n",
      file = outfile, append = TRUE)

}
