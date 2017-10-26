# this functions reads a dat and def file for ValueDriver and extracts the relevant information
# uses: VD.read_def()



#' Import ValueDriver data and definitions
#' 
#' Imports the dat/def files for a ValueDriver study and extracts the relevant
#' information.
#' 
#' 
#' @param dat_file A string value with the path to the DAT file to import.
#' @param def_file A string value with the path to the DEF file to import.
#' @param nlev A vector indicating the number of levels per attribute
#' @param none A boolean variable indicating whether NONE is included in the
#' dat file or not - default = \code{TRUE}
#' @return A list including elements \item{dat}{A matrix of the imported
#' dat-file} \item{utils_mat}{A matrix including the utilities from the dat
#' file} \item{utils_list}{A list including the individual utilities from the
#' dat file. One list element per respondent} \item{seg}{A matrix containing
#' the segment data} \item{weight}{A vector containing the weight per
#' respondent} \item{def}{A list containing the values from
#' \code{\link{VD.read_def}}} \item{nlev}{A vector containing the number of
#' levels per attribute.} \item{nseg}{A variable indicating the number of
#' segments in the dat file} \item{ID}{A vector containing the ID per
#' respondent} \item{RLH}{A vector containing the root likelihood (RLH) from
#' the HB estiomation per respondent.} \item{check1}{check if the extracted
#' info is consistent with the dat file (number of segments)}
#' \item{check2}{check if the extracted info is consistent with the dat file
#' (number of total levels)}
#' @author Maximilian Rausch - Maximilian.Rausch@@tns-infratest.com
#' @examples
#' 
#' \dontrun{
#' VDdata <- get_DriverData(dat_file = "TEST_timtim_gew.dat",
#'                          def_file = "TEST_timtim_gew.def",
#'                          nlev = c(4, 6, 4, 2, 2, 2, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5),
#'                          none = TRUE)
#' str(VDdata)
#' }
#' 
#' @export get_DriverData
get_DriverData <- function(dat_file = NULL, def_file = NULL, nlev = NULL, none = TRUE) {

  if (is.null(dat_file)) {
    stop("You need to specifiy a dat-file")
  }
  if (is.null(def_file)) {
    stop("You need to specifiy a def-file")
  }
  if (is.null(nlev)) {
    stop("You need to specifiy the number of levels per attribute")
  }

  ## read def-file
  def <- VD.read_def(def_file, nlev)
  nseg <- def$nseg
  natt <- length(nlev)

  col_dat <- 5 + sum(nlev) + none*1 + 1 + nseg

  ## read dat-file and label it in case of standard dat-file
  dat <- matrix(scan(dat_file), ncol=col_dat, byrow=TRUE)
  if (none) {
    colnames(dat) <- c("ID", "RLH", "nAttr", "nLev", "nSeg", paste("Att", rep(sequence(natt), nlev), "Lev", sequence(nlev), sep=""), "none", "weight", paste("seg", 1:nseg, sep=""))
  }
  else {
    colnames(dat) <- c("ID", "RLH", "nAttr", "nLev", "nSeg", paste("Att", rep(sequence(natt), nlev), "Lev", sequence(nlev), sep=""), "weight", paste("seg", 1:nseg, sep=""))
  }
  check1 <- length(def$att_List) == dat[1,3]
  check2 <- sum(nlev) == dat[1,4]

  weight <- dat[,"weight"]

  ## define indicies of the utility values (with or without NONE)
  ifelse(none, utils_ind <- 6:(sum(nlev)+6), utils_ind <- 6:(sum(nlev)+5))

  ## extract utilities to list
  utils_list <- split(dat[,utils_ind], seq_along(dat[,1]))

  ## extract utilities to matrix
  utils_mat <- dat[,utils_ind]

  seg_ind <- which(colnames(dat) %in% paste("seg", 1:nseg, sep=""))
  seg_dat <- dat[,seg_ind]

  invisible(list(dat = dat,
                 utils_mat = utils_mat,
                 utils_list = utils_list,
                 seg = seg_dat,
                 weight = weight,
                 def = def,
                 nlev = nlev,
                 nseg = nseg,
                 ID = dat[,1],
                 RLH = dat[,2],
                 check1 = check1,
                 check2 = check2))
}
