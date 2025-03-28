# read pricer dat-file and def-file and extract relevant parts

#' Import BMS Pricer data and definitions
#'
#' Imports the dat/def files for a BMS Pricer study and extracts the relevant information.
#'
#'
#' @param dat_file A string value with the path to the DAT file to import.
#' @param def_file A string value with the path to the DEF file to import.
#' @param none A boolean variable indicating whether NONE is included in the
#' dat file or not - default = \code{TRUE}
#' @return A list including elements
#'   \item{dat}{A matrix of the imported dat-file}
#'   \item{utils_mat}{A matrix including the utilities from the dat file}
#'   \item{utils_list}{A list including the individual utilities from the dat file. One list element per respondent}
#'   \item{iaw}{A matrix including the individual awareness factors from the dat file.}
#'   \item{idis}{A matrix including the individual distribution factors from the dat file.}
#'   \item{seg}{A matrix containing the segment data}
#'   \item{weight}{A vector containing the weight per respondent}
#'   \item{def}{A list containing the values from \code{\link{VP.read_def}}}
#'   \item{priceList}{A list containing the prices used in the model}
#'   \item{SKUs}{A vector containing the SKU labels from the def-file.}
#'   \item{nlev}{A vector containing the number of levels per attribute.}
#'   \item{ID}{A vector containing the ID per respondent}
#' @author Maximilian Rausch - Maximilian.Rausch@@bms-net.de
#' @examples
#'
#' \dontrun{
#' beer_data <- BMS.get_PricerData(dat_file = "beer_study.dat",
#'                             def_file = "beer_study.def",
#'                             none = TRUE)
#'
#' str(beer_data)
#' }
#'
#' @export BMS.get_PricerData
BMS.get_PricerData <- function(dat_file = NULL, def_file = NULL, none = TRUE) {

  if (is.null(dat_file)) {
    stop("You need to specifiy a dat-file")
  }
  if (is.null(def_file)) {
    stop("You need to specifiy a def-file")
  }

  ## read def-file
  def <- BMS.read_def(def_file)
  nseg <- length(def$segDef)

  ## extract the number of SKUs and price points
  nlev <- c(length(def$brands), unlist(lapply(def$prices, length)))

  col_dat <- 1 + sum(nlev) + none[1] * 1 + nseg + 1 + 2 * (length(def$brands) + 1)

  ## read dat-file and label it in case of standard dat-file
  dat <- matrix(scan(dat_file, quiet = TRUE), ncol = col_dat, byrow = TRUE)
  if (none[1]) {
    colnames(dat) <- c("ID", paste("SKU", sequence(nlev[1]), sep = ""),
                       paste("p", rep(sequence(nlev[1]), nlev[-1]), "l",
                             sequence(nlev[-1]), sep = ""), "none", paste("seg", 1:nseg, sep = ""), "gew",
                       paste("iaw", sequence(length(def$brands)), sep = ""), "iawnone",
                       paste("idis", sequence(length(def$brands)), sep = ""), "idisnone")
  }
  else {
    colnames(dat) <- c("ID", paste("SKU", sequence(nlev[1]), sep = ""),
                       paste("p", rep(sequence(nlev[1]), nlev[-1]), "l", sequence(nlev[-1]), sep = ""),
                       paste("seg", 1:nseg, sep = ""), "gew", paste("iaw", sequence(length(def$brands)), sep = ""),
                       paste("idis", sequence(length(def$brands)), sep = ""))
  }

  weight <- dat[, "gew"]

  ## define indicies of the utility values (with or without NONE)
  utils_ind <- 2:(sum(nlev) + ifelse(none, 2, 1))

  ## extract utilities to list
  utils_list <- split(dat[, utils_ind], dat[, 1])

  ## extract utilities to matrix
  utils_mat <- dat[, utils_ind]

  ## iaw and idis
  iaw_ind <- grep('^iaw', colnames(dat))
  idis_ind <- grep('^idis', colnames(dat))

  iaw <- dat[, iaw_ind]
  idis <- dat[, idis_ind]

  ## segment data
  seg_ind <- grep('^seg', colnames(dat))

  seg_dat <- dat[, seg_ind]

  invisible(list(dat = dat,
                 utils_mat = utils_mat,
                 utils_list = utils_list,
                 iaw = iaw,
                 idis = idis,
                 seg = seg_dat,
                 weight = weight,
                 def = def,
                 priceList = def$prices,
                 SKUs = def$brands,
                 nlev = nlev,
                 ID = dat[, 1]))
}
