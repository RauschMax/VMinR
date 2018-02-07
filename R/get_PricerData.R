# read pricer dat-file and def-file and extract relevant parts



#' Import ValuePricer data and definitions
#'
#' Imports the dat/def files for a ValuePricer study and extracts the relevant
#' information.
#'
#'
#' @param dat_file A string value with the path to the DAT file to import.
#' @param def_file A string value with the path to the DEF file to import.
#' @param none A boolean variable indicating whether NONE is included in the
#' dat file or not - default = \code{TRUE}
#' @return A list including elements \item{dat}{A matrix of the imported
#' dat-file} \item{utils_mat}{A matrix including the utilities from the dat
#' file} \item{utils_list}{A list including the individual utilities from the
#' dat file. One list element per respondent} \item{iaw}{A matrix including the
#' individual awareness factors from the dat file.} \item{idis}{A matrix
#' including the individual distribution factors from the dat file.}
#' \item{seg}{A matrix containing the segment data} \item{weight}{A vector
#' containing the weight per respondent} \item{def}{A list containing the
#' values from \code{\link{VP.read_def}}} \item{pricemat_tested}{A matrix
#' containing the prices used in the model} \item{pricerange_tested}{A vector
#' containing the minimal and maximal price used in the model}
#' \item{pr_range_mat}{A matrix containing the minimal and maximal prices per
#' SKU used in the model} \item{SKUs}{A vector containing the SKU labels from
#' the def-file.} \item{nlev}{A vector containing the number of levels per
#' attribute.} \item{ID}{A vector containing the ID per respondent}
#' @author Maximilian Rausch - Maximilian.Rausch@@tns-infratest.com
#' @examples
#'
#' \dontrun{
#' beer_data <- get_PricerData(dat_file = "beer_study.dat",
#'                             def_file = "beer_study.def",
#'                             none = TRUE)
#'
#' str(beer_data)
#' }
#'
#' @export get_PricerData
get_PricerData <- function(dat_file = NULL, def_file = NULL, none = TRUE) {

  if (is.null(dat_file)) {
    stop("You need to specifiy a dat-file")
  }
  if (is.null(def_file)) {
    stop("You need to specifiy a def-file")
  }

  ## read def-file
  def <- VP.read_def(def_file)

  nseg <- def$nseg

  ## extract prices tested
  pricemat_tested <- def$prices
  pricerange_tested <- c(min(pricemat_tested), max(pricemat_tested))
  pr_range_mat <- t(apply(pricemat_tested, 1, range))

  ## extract the number of SKUs and price points
  nlev <- c(length(def$brands), rep(dim(def$prices)[2], dim(def$prices)[1]))

  col_dat <- 1 + sum(nlev) + none[1]*1 + nseg + 1 + 2 * (length(def$brands)+1)

  ## read dat-file and label it in case of standard dat-file
  dat <- matrix(scan(dat_file, quiet = TRUE), ncol=col_dat, byrow=TRUE)
  if (none[1]) {
    colnames(dat) <- c("ID", paste("SKU", sequence(nlev[1]), sep=""), paste("p", rep(sequence(nlev[1]), nlev[-1]), "l", sequence(nlev[-1]), sep=""), "none", paste("seg", 1:nseg, sep=""), "gew", paste("iaw", sequence(length(def$brands)), sep=""), "iawnone", paste("idis", sequence(length(def$brands)), sep=""), "idisnone")
  }
  else {
    colnames(dat) <- c("ID", paste("SKU", sequence(nlev[1]), sep=""), paste("p", rep(sequence(nlev[1]), nlev[-1]), "l", sequence(nlev[-1]), sep=""), paste("seg", 1:nseg, sep=""), "gew", paste("iaw", sequence(length(def$brands)), sep=""), paste("idis", sequence(length(def$brands)), sep=""))
  }

  weight <- dat[,"gew"]

  ## define indicies of the utility values (with or without NONE)
  ifelse(none, utils_ind <- 2:(sum(nlev)+2), utils_ind <- 2:(sum(nlev)+1))

  ## extract utilities to list
  utils_list <- split(dat[,utils_ind], dat[,1])

  ## extract utilities to matrix
  utils_mat <- dat[,utils_ind]

  ifelse(none, iaw_ind <- which(colnames(dat) %in% c(paste("iaw", sequence(length(def$brands)), sep=""), "iawnone")),
         iaw_ind <- which(colnames(dat) %in% c(paste("iaw", sequence(length(def$brands)), sep=""))))
  ifelse(none, idis_ind <- which(colnames(dat) %in% c(paste("idis", sequence(length(def$brands)), sep=""), "idisnone")),
         idis_ind <- which(colnames(dat) %in% c(paste("idis", sequence(length(def$brands)), sep=""))))

  iaw <- dat[,iaw_ind]
  idis <- dat[,idis_ind]

  seg_ind <- which(colnames(dat) %in% paste("seg", 1:nseg, sep=""))

  seg_dat <- dat[,seg_ind]

  invisible(list(dat = dat,
                 utils_mat = utils_mat,
                 utils_list = utils_list,
                 iaw = iaw,
                 idis = idis,
                 seg = seg_dat,
                 weight = weight,
                 def = def,
                 pricemat_tested = pricemat_tested,
                 pricerange_tested = pricerange_tested,
                 pr_range_mat = pr_range_mat,
                 SKUs = def$brands,
                 nlev = nlev,
                 ID = dat[,1]))
}
