# this functions reads a dat and def file for ValueDriver and extracts the relevant information
# uses: VD.read_def()

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
