#' Correlation heatmap
#'
#' Calculates correlation heatmap and MDS coordinates for ValuePricer projects.
#'
#'
#' @param input_file the path to the heatmap input file. A specific file which
#' contains all the necessary information. (See VM sharepoint)
#' @param sepOUT separator for output csv-files - default = \code{";"}
#' @param decOUT decimal sign for output csv-files - default = \code{","}
#' @param usedraws A boolean variable indicating whether DRAWS should be used
#' as well or not - default = \code{FALSE}
#' @param clustered A boolean variable indicating whether clustered heatmaps
#' should be calculated or not - default = \code{FALSE}
#' @return A list including 23 elements \item{input_file}{the path to the
#' heatmap input file.} \item{cor}{A matrix of the correlations for the
#' scenario specified in the input file.} \item{cor_draws}{A matrix of the
#' correlations for the scenario specified in the input file based on the
#' DRAWS. If \code{usedraws == TRUE}} \item{cor_clustered}{A matrix of the
#' correlations for the scenario specified in the input file for the CLUSTERS.
#' If \code{clustered == TRUE}} \item{cor_draws_clustered}{A matrix of the
#' correlations for the scenario specified in the input file for the CLUSTERS
#' based on the DRAWS If \code{usedraws == TRUE} and \code{clustered == TRUE}}
#' \item{base_sim}{A vector with the unweighted aggregated shares for the
#' scenario specified in the input file.} \item{base_sim_draws}{A vector with
#' the unweighted aggregated shares for the scenario specified in the input
#' file based on the DRAWS. If \code{usedraws == TRUE}} \item{draws}{A matrix
#' containing the draws used for the study} \item{utls}{A matrix containing the
#' utilities used for the study} \item{Xbeta}{\code{X * beta} matrix (utility
#' sums).} \item{Xbeta_draws}{\code{X * beta} matrix for the draws (utility
#' sums). If \code{usedraws == TRUE}} \item{Xbeta_clustered}{\code{X * beta}
#' matrix (utility sums). If \code{clustered == TRUE}}
#' \item{Xbeta_draws_clustered}{\code{X * beta} matrix for the draws (utility
#' sums). If \code{usedraws == TRUE} and \code{clustered == TRUE}}
#' \item{brand_list}{A list with one item per cluster containing the respectiv
#' indicies for the SKUs of the clusters.} \item{MDS_coord}{A matrix containing
#' the MDS coordinates} \item{MDS_coord_draws}{A matrix containing the MDS
#' coordinates for DRAWS. If \code{usedraws == TRUE}}
#' \item{MDS_coord_clustered}{A matrix containing the clustered MDS
#' coordinates. If \code{clustered == TRUE}} \item{MDS_coord_draws_clustered}{A
#' matrix containing the clustered MDS coordinates for DRAWS. If \code{usedraws
#' == TRUE} and \code{clustered == TRUE}} \item{SKUlabels}{A vector with the
#' SKU labels passed by the input file.} \item{ClusterLabels}{A vector with the
#' cluster labels passed by the input file.} \item{prices}{A matrix containing
#' the prices used in the model.} \item{simPrices}{A vector containing the
#' prices used for the heatmap calculation.} \item{simSKUs}{A vector with the
#' SKU indicies used for the heatmap calculation.}
#' @author Maximilian Rausch - Maximilian.Rausch@@tns-infratest.com
#' @examples
#'
#' \dontrun{
#' heat <- correlation_HeatMap("Input_DUMMY.txt",
#'                             usedraws=FALSE,
#'                             clustered = FALSE,
#'                             sepOUT=",",
#'                             decOUT=".")
#' }
#'
#' @export correlation_HeatMap
correlation_HeatMap <- function(input_file = NULL, sepOUT = ";", decOUT = ",", usedraws = FALSE, clustered = FALSE) {
  if (is.null(input_file)) stop("Please specify the path to the input file!")

  # read input file and define the skip parameters for reading the data files
  input <- scan(input_file, what = "character", sep = "\n", strip.white = TRUE, comment.char = "*",
                blank.lines.skip = FALSE, quiet = TRUE)
  skip_hbu  <- which(input == "[DAT-File]")
  if (length(skip_hbu) == 0) stop(paste("Please check your input file", input_file,
                                        ". The part [DAT-File] is missing!"))
  skip_draw <- which(input == "[DRAW-File]")
  if (length(skip_draw) == 0) stop(paste("Please check your input file", input_file,
                                         ". The part [DRAW-File] is missing!"))
  skip_def <- which(input == "[DEF-File]")
  if (length(skip_def) == 0) stop(paste("Please check your input file", input_file,
                                        ". The part [DEF-File] is missing!"))
  skip_base <- which(input == "[BaseCase]")
  if (length(skip_base) == 0) stop(paste("Please check your input file", input_file,
                                         ". The part [BaseCase] is missing!"))
  skip_out  <- which(input == "[Output-Name]")
  if (length(skip_out) == 0) stop(paste("Please check your input file", input_file,
                                        ". The part [Output-Name] is missing!"))
  skip_cluster  <- which(input == "[Clusters]")
  if (length(skip_cluster) == 0) stop(paste("Please check your input file", input_file,
                                            ". The part [Clusters] is missing!"))
  skip_names  <- which(input == "[ClusterNames]")
  if (length(skip_names) == 0) stop(paste("Please check your input file", input_file,
                                          ". The part [ClusterNames] is missing!"))
  skip_SKU  <- which(input == "[SKUNames]")
  if (length(skip_SKU) == 0) stop(paste("Please check your input file", input_file,
                                        ". The part [SKUNames] is missing!"))

  # read number of levels and attributes from the hbu-file
  if (length(which(input == "[nSKUs]")) == 0) stop(paste("Please check your input file",
                                                         input_file, ". The part [nSKUs] is missing!"))
  if (length(which(input == "[nPrices]")) == 0) stop(paste("Please check your input file",
                                                           input_file, ". The part [nPrices] is missing!"))
  nlev <- c(as.numeric(input[which(input == "[nSKUs]") + 1]),
            rep(as.numeric(input[which(input == "[nPrices]") + 1]), as.numeric(input[which(input == "[nSKUs]") + 1])))
  # nattr <- length(nlev)
  nresp <- length(as.matrix(data.table::fread(input[skip_hbu + 1], sep = "\n", header = FALSE)))

  cat("reading Data \n") # read dat-file
  hbu <- matrix(scan(input[skip_hbu + 1], quiet = TRUE), nrow = nresp, byrow = TRUE)[, 1:(sum(nlev) + 2)]
  hbu <- hbu[order(hbu[, 1]), ]
  colnames(hbu) <- c("respnum", paste("SKU_", 1:(nlev[1]), sep = ""),
                     paste("p", rep(1:(nlev[1]), nlev[-1]), "_", sequence(nlev[-1]), sep = ""), "none")

  cat("reading DEF-file \n")
  def <- VP.read_def(input[skip_def + 1])

  draws <- NA
  if (usedraws) {
    cat("reading DRAWS \n")  # read draw-file if specified
    draws <- as.matrix(data.table::fread(input[skip_draw + 1], skip = 1))
    #draws <- matrix(scan(input[skip_draw + 1], skip=1, sep=sepIN, dec=decIN, comment.char = "*", quiet = TRUE),
    #ncol=(sum(nlev) + 4), byrow=T)
    draws <- draws[order(draws[, 1]), ]
    colnames(draws) <- c("respnum", "draw", "RLH", paste("SKU_", 1:(nlev[1]), sep = ""),
                         paste("p", rep(1:(nlev[1]), nlev[-1]), "_", sequence(nlev[-1]), sep = ""), "none")
  }

  cat("Defining BaseCase \n")
  if (input[skip_base + 1] == 0) {
    # default base case - all prices set to 3rd level
    simPrices <- def$prices[, 3]
    simSKUs <- seq_along(simPrices)
  }
  else {
    # read prespecified base case and convert to dummy coded matrix
    simIN <- matrix(scan(input_file, skip = skip_base, nlines = skip_out - skip_base - 1,
                         comment.char = "*", quiet = TRUE), ncol = 2, byrow = TRUE)
    simPrices <- simIN[, 2]
    simSKUs <- simIN[, 1]
  }

  # read cluster information
  brand_list <- NA
  if (clustered) {
    cat("Reading cluster information \n")
    brand_mat <- matrix(scan(input_file, skip = skip_cluster, nlines = skip_names - skip_cluster - 1,
                             comment.char = "*", quiet = TRUE), ncol = 2, byrow = TRUE)
    if (!identical(sort(brand_mat[, 2]), sort(simSKUs))) {
      stop("The SKUs used in the clusters need to be identical to the ones in the basecase")
      }
    brand_list <- split(seq_along(brand_mat[, 2]), brand_mat[, 1])
    names(brand_list) <- input[(skip_names + 1):(skip_SKU - 1)]
  }

  # read SKU labels
  SKUlabels <- input[1:length(simSKUs) + skip_SKU]

  # matrix multiplication X*beta - i.e. calculate utility sums for each concept of the base case
  cat("simBase \n")
  simBase <- VP.computeShares(hbu[, -1], prices = def$prices, simPrices = simPrices, simSKUs = simSKUs, nlev = nlev,
                              weight = NULL, none = FALSE, iaw = NULL, FC = FALSE)

  # set BaseCase_share_draws to NA; overwrite in case draws are included
  # BaseCase_share_draws <- NA
  # sim_basecase_draws <- NA
  simBase_draws <- list(ind_sim = NA)
  # Xbeta_draws <- NA
  Xbeta_clustered <- NA
  Xbeta_draws_clustered <- NA
  BaseCase_cor_clustered <- NA
  BaseCase_cor_draws <- NA
  BaseCase_cor_draws_clustered <- NA
  mds_draws <- NA
  mds_clustered <- NA
  mds_draws_clustered <- NA
  if (usedraws) {
    simBase_draws <- VP.computeShares(draws[, -(1:3)], prices = def$prices, simPrices = simPrices, simSKUs = simSKUs,
                                      nlev = nlev, weight = NULL, none = FALSE, iaw = NULL, FC = FALSE)
  }

  cat("Calculating correlations \n") # calculate correlations
  BaseCase_cor <- stats::cor(t(simBase$Xbeta))
  colnames(BaseCase_cor) <- SKUlabels
  rownames(BaseCase_cor) <- SKUlabels

  # MDS-Koordinaten
  mds <- stats::cmdscale(stats::as.dist((1 - BaseCase_cor) / 2))

  if (usedraws) {
    BaseCase_cor_draws <- stats::cor(t(simBase_draws$Xbeta)) # calculate correlations for draws if applicable
    colnames(BaseCase_cor_draws) <- SKUlabels
    rownames(BaseCase_cor_draws) <- SKUlabels

    # MDS-Koordinaten
    mds_draws <- stats::cmdscale(stats::as.dist((1 - BaseCase_cor_draws) / 2))
  }

  if (clustered) {
    Xbeta_clustered <- matrix(NA, nrow = length(brand_list), ncol = dim(simBase$Xbeta)[2])
    for (i in seq_along(names(brand_list))) {
      Xbeta_clustered[i, ] <- colSums(matrix(simBase$Xbeta[brand_list[[i]], ], ncol = dim(simBase$Xbeta)[2]))
    }

    cat("Calculating clustered correlations \n") # calculate correlations
    BaseCase_cor_clustered <- stats::cor(t(Xbeta_clustered))
    colnames(BaseCase_cor_clustered) <- names(brand_list)
    rownames(BaseCase_cor_clustered) <- names(brand_list)

    # MDS-Koordinaten
    mds_clustered <- stats::cmdscale(stats::as.dist((1 - BaseCase_cor_clustered) / 2))

    if (usedraws) {
      Xbeta_draws_clustered <- matrix(NA, nrow = length(brand_list), ncol = dim(simBase_draws$Xbeta)[2])
      for (i in seq_along(names(brand_list))) {
        Xbeta_draws_clustered[i, ] <- colSums(matrix(simBase_draws$Xbeta[brand_list[[i]], ],
                                                     ncol = dim(simBase_draws$Xbeta)[2]))
      }

      # calculate correlations for draws if applicable
      BaseCase_cor_draws_clustered <- stats::cor(t(Xbeta_draws_clustered))
      colnames(BaseCase_cor_draws_clustered) <- names(brand_list)
      rownames(BaseCase_cor_draws_clustered) <- names(brand_list)

      # MDS-Koordinaten
      mds_draws_clustered <- stats::cmdscale(stats::as.dist((1 - BaseCase_cor_draws_clustered) / 2))
    }

  }

  # write Correlogram into PDF-file.
  cat("Creating correlograms \n")
  grDevices::pdf(file = paste(input[skip_out + 1], "_correlogram_BaseCase.pdf", sep = ""),
                 paper = "a4r", width = 25, height = 15, pointsize = 10)
  corrgram::corrgram(BaseCase_cor, order = NULL, lower.panel = corrgram::panel.bar,
           upper.panel = corrgram::panel.bar, text.panel = corrgram::panel.txt,
           main = paste(input[skip_out + 1], "- Correlogram BaseCase"))
  grDevices::dev.off()
  if (usedraws) {
    grDevices::pdf(file = paste(input[skip_out + 1], "_correlogram_BaseCase_DRAWS.pdf", sep = ""),
                   paper = "a4r", width = 25, height = 15, pointsize = 10)
    corrgram::corrgram(BaseCase_cor_draws, order = NULL, lower.panel = corrgram::panel.bar,
             upper.panel = corrgram::panel.bar, text.panel = corrgram::panel.txt,
             main = paste(input[skip_out + 1], "- Correlogram BaseCase DRAWS"))
    grDevices::dev.off()
  }

  if (clustered) {
    grDevices::pdf(file = paste(input[skip_out + 1], "_correlogram_BaseCase_clustered.pdf", sep = ""),
                   paper = "a4r", width = 25, height = 15, pointsize = 10)
    corrgram::corrgram(BaseCase_cor_clustered, order = NULL, lower.panel = corrgram::panel.bar,
             upper.panel = corrgram::panel.bar, text.panel = corrgram::panel.txt,
             main = paste(input[skip_out + 1], "- Correlogram BaseCase (clustered)"))
    grDevices::dev.off()
    if (usedraws) {
      grDevices::pdf(file = paste(input[skip_out + 1], "_correlogram_BaseCase_DRAWS_clustered.pdf", sep = ""),
                     paper = "a4r", width = 25, height = 15, pointsize = 10)
      corrgram::corrgram(BaseCase_cor_draws_clustered, order = NULL, lower.panel = corrgram::panel.bar,
               upper.panel = corrgram::panel.bar, text.panel = corrgram::panel.txt,
               main = paste(input[skip_out + 1], "- Correlogram BaseCase DRAWS (clustered)"))
      grDevices::dev.off()
    }
  }


  cat("Writing correlations \n") # write correlations into csv-file
  utils::write.table(BaseCase_cor, paste(input[skip_out + 1], "_correlation_BaseCase.csv", sep = ""),
                     sep = sepOUT, dec = decOUT, col.names = NA)
  utils::write.table(mds, paste(input[skip_out + 1], "_MDS_BaseCase.csv", sep = ""),
                     sep = sepOUT, dec = decOUT, col.names = FALSE, row.names = TRUE)

  if (usedraws) {
    utils::write.table(BaseCase_cor_draws, paste(input[skip_out + 1], "_correlation_BaseCase_DRAWS.csv", sep = ""),
                       sep = sepOUT, dec = decOUT, col.names = NA)
    utils::write.table(mds_draws, paste(input[skip_out + 1], "_MDS_BaseCase_DRAWS.csv", sep = ""),
                       sep = sepOUT, dec = decOUT, col.names = FALSE, row.names = TRUE)
  }
  if (clustered) {
    utils::write.table(BaseCase_cor_clustered,
                       paste(input[skip_out + 1], "_correlation_BaseCase_clustered.csv", sep = ""),
                       sep = sepOUT, dec = decOUT, col.names = NA)
    utils::write.table(mds_clustered, paste(input[skip_out + 1], "_MDS_BaseCase_clustered.csv", sep = ""),
                       sep = sepOUT, dec = decOUT, col.names = FALSE, row.names = TRUE)

    if (usedraws) {
      utils::write.table(BaseCase_cor_draws_clustered,
                         paste(input[skip_out + 1], "_correlation_BaseCase_DRAWS_clustered.csv", sep = ""),
                         sep = sepOUT, dec = decOUT, col.names = NA)
      utils::write.table(mds_draws_clustered, paste(input[skip_out + 1], "_MDS_BaseCase_DRAWS_clustered.csv", sep = ""),
                         sep = sepOUT, dec = decOUT, col.names = FALSE, row.names = TRUE)
    }
  }
  cat("DONE! \n")

  invisible(list(input_file = input_file,
                 cor = BaseCase_cor,
                 cor_draws = BaseCase_cor_draws,
                 cor_clustered = BaseCase_cor_clustered,
                 cor_draws_clustered = BaseCase_cor_draws_clustered,
                 base_sim = simBase$ind_sim,
                 base_sim_draws = simBase_draws$ind_sim,
                 draws = draws,
                 utls = hbu,
                 Xbeta = simBase$Xbeta,
                 Xbeta_draws = simBase_draws$Xbeta,
                 Xbeta_clustered = Xbeta_clustered,
                 Xbeta_draws_clustered = Xbeta_draws_clustered,
                 brand_list = brand_list,
                 MDS_coord = mds,
                 MDS_coord_draws = mds_draws,
                 MDS_coord_clustered = mds_clustered,
                 MDS_coord_draws_clustered = mds_draws_clustered,
                 SKUlabels = SKUlabels,
                 ClusterLabels = names(brand_list),
                 prices = def$prices,
                 simPrices = simPrices,
                 simSKUs = simSKUs))
}
