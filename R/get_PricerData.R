# read pricer dat-file and def-file and extract relevant parts

get_PricerData <- function(dat_file = NULL, def_file = NULL, nseg = NULL, none = TRUE) {

  if (is.null(dat_file)) {
    stop("You need to specifiy a dat-file")
  }
  if (is.null(def_file)) {
    stop("You need to specifiy a def-file")
  }
  if (is.null(nseg)) {
    stop("You need to specifiy the number of segments in the dat-file")
  }

  ## read def-file
  def <- VP.read_def(def_file)

  ## extract prices tested
  pricemat_tested <- def$prices
  pricerange_tested <- c(min(pricemat_tested), max(pricemat_tested))
  pr_range_mat <- t(apply(pricemat_tested, 1, range))

  ## extract the number of SKUs and price points
  nlev <- c(length(def$brands), rep(dim(def$prices)[2], dim(def$prices)[1]))

  col_dat <- 1 + sum(nlev) + none[1]*1 + nseg + 1 + 2 * (length(def$brands)+1)

  ## read dat-file and label it in case of standard dat-file
  dat <- matrix(scan(dat_file), ncol=col_dat, byrow=TRUE)
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
