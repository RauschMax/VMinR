#' Concept optimization with ISBC
#'
#' Runs the process for "concept optimization with ISBC" studies - Always weighted!
#'
#'
#' @param dat A string indicating the path/filename of the ValueDriver dat-file.
#' @param def A string indicating the path/filename of the ValueDriver def-file.
#' @param FrameData A string indicating the path/filename of the frame questionnaire data.
#' @param calib An boolean value indicating if the utilities should be calibrated on purchase
#' intention - default = \code{TRUE}.
#' @param nlev A vector indicating the number of levels per attribute
#' @param cut An integer value indicating the maximal value for utilities after calibration.
#' (required for \code{\link{calibEXE}})
#' @param ID A string indicating the name of teh ID variable in the SPSS dataset - default = \code{"Respondent_Serial"}.
#' @return A list including 13 elements
#' \item{call}{A string returning the call including the inputs}
#' \item{calibData}{A list with the values of the \code{\link{calibEXE}} call.
#' (\code{NULL} if \code{calib == FALSE}))}
#' \item{excel_export}{A data.frame containing the export matrix;
#' concept definition + aggregated shares + sd + individual shares.}
#' \item{excel_export_calib}{A data.frame containing the calibrated export matrix;
#' concept definition + aggregated shares + sd + individual shares. (\code{NULL} if \code{calib == FALSE}))}
#' \item{dat_file_calib}{A data.frame containing the export information for the
#' calibrated ValueDriver dat-file. (\code{NULL} if \code{calib == FALSE}))}
#' \item{overview}{A data.frame with additional information on the calibExe step.
#' (\code{NULL} if \code{calib == FALSE}))}
#' \item{path}{A string returning the working directory where the export files had been saved to.}
#' \item{SPSSwarn}{A vector with potential warnings during the SPSS import.
#' These usually can be ignored}
#' @author Maximilian Rausch - Maximilian.Rausch@tns-infratest.com
#' @examples
#'
#' \dontrun{
#' nlev <- c(4, 4, 4, 2, 4, 2, 4, 6)
#' ConcOptTest <- ConceptOpt_ISBC("Innogy.dat",
#'                                "Innogy.def",
#'                                FrameData = "./data_calib.SAV",
#'                                calib = TRUE,
#'                                nlev = nlev,
#'                                cut = 42,
#'                                ID = "Respondent_Serial")
#' str(ConcOptTest)
#' }
#'
#' @export ConceptOpt_ISBC



ConceptOpt_ISBC <- function(dat = NULL, def = NULL, FrameData = NULL, calib = TRUE, nlev = NULL, cut = 42, ID = "Respondent_Serial") {
  # some checks ----
  if (is.null(dat)) stop("Please provide the path and filename of the ValueDriver dat-file!")
  if (is.null(def)) stop("Please provide the path and filename of the ValueDriver def-file!")
  if (is.null(FrameData)) stop("Please the path and filename of the frame questionnaire data!")
  if (is.null(nlev)) stop("Please provide vector of number of levels!")

  callAkt <- match.call(expand.dots = TRUE)

  natt <- length(nlev)

  # read dat/def file (ValueDriver structure) ----
  dat_input <- get_DriverData(dat, def, nlev = nlev, none = TRUE)

  # read frame questionnaire - IF calib = TRUE
  if (calib) {

    # frame_data <- suppressWarnings(read.spss(FrameData, to.data.frame = TRUE))
    frame_data <- foreign::read.spss(FrameData, to.data.frame = TRUE)

    # SPSSwarn <- warnings()

    best_concepts <- t(matrix(as.numeric(unlist(lapply(as.character(frame_data$"VM_DCM_Driver_Block1_ISBC_BestConcept"),
                                                       strsplit, split = "[,]"))), nrow = natt)) + 1

    worst_concepts <- t(matrix(as.numeric(unlist(lapply(as.character(frame_data$"VM_DCM_Driver_Block1_ISBC_WorstConcept"),
                                                        strsplit, split = "[,]"))), nrow = natt)) + 1

    purch_best <- frame_data$"VM_DCM_Driver_Block1_VM_DCM_BestWorst_best_PurchaseIntention"

    purch_worst <- frame_data$"VM_DCM_Driver_Block1_VM_DCM_BestWorst_worst_PurchaseIntention"

    BWconcepts <- as.data.frame(cbind(frame_data[ID], best_concepts, worst_concepts, purch_best, purch_worst))
    names(BWconcepts) <- c("ID",
                           paste0("B_Att_", sequence(natt)),
                           paste0("W_Att_", sequence(natt)),
                           "PI_B", "PI_W")

    # calibeEXE step
    calibData <- calibEXE(BWconcepts = BWconcepts[, c(paste0("B_Att_", sequence(natt)), paste0("W_Att_", sequence(natt)))],
                          PI = BWconcepts[, c("PI_B", "PI_W")],
                          utils = dat_input$utils_mat,
                          cut = cut, nlev = nlev)
  }

  # create all possible concepts
  allConc <- AlgDesign::gen.factorial(levels = nlev, nVars = natt, center = FALSE)

  # recodes the concepts into DUMMY (0/1) coding ----
  allConc_dummy <- as.matrix(convertSSItoDesign(allConc, nlev = nlev))

  # applies the product acceptance for all concepts on individual level
  shares_ProdAcc <- apply(allConc_dummy, 1, prodAcceptance,
                          utils = dat_input$utils_mat[,-ncol(dat_input$utils_mat)])

  if (calib) {
    shares_ProdAcc_calib <- apply(allConc_dummy, 1, prodAcceptance,
                                  utils = as.matrix(calibData$utils_calib[,-ncol(calibData$utils_calib)]))
  }

  # aggregated product acceptance
  agg_shares <- round(apply(shares_ProdAcc, 2, weighted.mean, w = dat_input$weight), 3)

  if (calib) {
    agg_shares_calib <- round(apply(shares_ProdAcc_calib, 2, weighted.mean, w = dat_input$weight), 3)
  }

  # export histogram and comparison-csv if calib = TRUE
  if (calib) {
    hist_dat <- data.frame(agg_shares, agg_shares_calib)

    hist_dat_reshape <- stats::reshape(hist_dat, dir = 'long', varying = list(1:2), v.names = "agg_shares")
    hist_dat_reshape$time <- as.factor(hist_dat_reshape$time)
    levels(hist_dat_reshape$time) <- c("agg_shares", "agg_shares_calib")

    # Histogram with density curve
    # requireNamespace("ggplot2")
    ggplot2::ggplot(data = hist_dat_reshape, ggplot2::aes(x = agg_shares)) +
      ggplot2::geom_histogram(colour = "black", binwidth = .01, ggplot2::aes(fill = hist_dat_reshape$time, y = ..density..)) +
      ggplot2::geom_density(size=1) +
      ggplot2::facet_grid(time ~ .) +
      ggplot2::theme(legend.position="none")        # No legend (redundant in this graph)
    ggplot2::ggsave("Histograms_agg_shares_calibEXE.pdf",
           plot = ggplot2::last_plot(),
           width = 20, height = 11,
           units = "in",
           dpi = 300)

    #### EXPORT RESULTS FOR COMPARISON ####

    best_dummy <- as.matrix(convertSSItoDesign(calibData$BWconcepts[, 1:(ncol(calibData$BWconcepts) / 2)],
                                               nlev = calibData$nlev))
    worst_dummy <- as.matrix(convertSSItoDesign(
      calibData$BWconcepts[, (1 + ncol(calibData$BWconcepts) / 2):ncol(calibData$BWconcepts)],
      nlev = calibData$nlev))

    ProdAcc_best <- exp(rowSums(best_dummy * calibData$utils[,-ncol(calibData$utils)])) /
      (1 + exp(rowSums(best_dummy * calibData$utils[,-ncol(calibData$utils)])))

    ProdAcc_best_calib <- exp(rowSums(best_dummy * calibData$utils_calib[,-ncol(calibData$utils_calib)])) /
      (1 + exp(rowSums(best_dummy * calibData$utils_calib[,-ncol(calibData$utils_calib)])))

    ProdAcc_worst <- exp(rowSums(worst_dummy * calibData$utils[,-ncol(calibData$utils)])) /
      (1 + exp(rowSums(worst_dummy * calibData$utils[,-ncol(calibData$utils)])))

    ProdAcc_worst_calib <- exp(rowSums(worst_dummy * calibData$utils_calib[,-ncol(calibData$utils_calib)])) /
      (1 + exp(rowSums(worst_dummy * calibData$utils_calib[,-ncol(calibData$utils_calib)])))

    export1 <- data.frame(dat_input$ID, calibData$utl_sum, calibData$check_order_BW,
                          calibData$PurchaseInt, calibData$check_order_PI,
                          apply(calibData$PurchaseInt, 1, diff),
                          round(ProdAcc_best_calib, 3), round(ProdAcc_best, 3),
                          round(ProdAcc_worst_calib, 3), round(ProdAcc_worst, 3),
                          calibData$a, calibData$b, BWconcepts[, -1])
    names(export1) <- c("ID", "utl_sum best", "utl_sum worst", "Check Order UTLs",
                        "PI best", "PI worst", "Check Order PI", "Difference PI",
                        "ProdAcc best calib", "ProdAcc best",
                        "ProdAcc worst calib", "ProdAcc worst",
                        "a", "b", names(BWconcepts)[-1])

    # write csv-files
    utils::write.table(export1, "calibExe_Overview.csv", row.names = FALSE, sep = ";", dec = ",")

    # summary ----
    summary_calibEXE(calibData, "summary_calibEXE.txt", infile = dat_input$def$file_in)
  }


  # write ValueDriver dat-file with calibrated utilities
  if (calib) {
    dat_file_calib <- cbind(dat_input$ID, dat_input$RLH, length(dat_input$nlev), sum(dat_input$nlev), dat_input$nseg,
                            round(calibData$utils_calib, 3), dat_input$weight, dat_input$seg)

    utils::write.table(dat_file_calib, file = paste0(strsplit(dat, "[.]dat"), "_calibEXE.dat"),
                col.names = FALSE, row.names = FALSE, sep = " ")
  }


  # Export matrix; concept definition + aggregated shares + sd + individual shares
  excel_export <- cbind(allConc,
                        agg_shares,
                        round(apply(shares_ProdAcc, 2, stats::sd), 3),
                        round(t(shares_ProdAcc), 3))

  colnames(excel_export) <- c(paste("Att", seq_along(nlev)), "ProdAcc ALL", "ProdAcc SD",
                              paste("ProdAcc Resp", 1:dim(shares_ProdAcc)[1], sep=""))

  # EXPORT csv-file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  utils::write.table(excel_export, "ProdAcc_Overview.csv", sep = ";", dec = ",", col.names = NA)

  if (calib) {
    excel_export_calib <- cbind(allConc,
                                agg_shares_calib,
                                round(apply(shares_ProdAcc_calib, 2, stats::sd), 3),
                                round(t(shares_ProdAcc_calib), 3))

    colnames(excel_export_calib) <- c(paste("Att", seq_along(nlev)), "ProdAcc ALL", "ProdAcc SD",
                                      paste("ProdAcc Resp", 1:dim(shares_ProdAcc_calib)[1], sep=""))

    # EXPORT csv-file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    utils::write.table(excel_export_calib, "ProdAcc_Overview_calib.csv", sep = ";", dec = ",", col.names = NA)
  }

  if (!calib) calibData <- NULL
  if (!calib) excel_export_calib <- NULL
  if (!calib) dat_file_calib <- NULL
  if (!calib) export1 <- NULL

  invisible(list(call = callAkt,
                 calibData = calibData,
                 excel_export = excel_export,
                 excel_export_calib = excel_export_calib,
                 dat_file_calib = dat_file_calib,
                 overview = export1,
                 path = getwd(),
                 weight = dat_input$weight))

}
