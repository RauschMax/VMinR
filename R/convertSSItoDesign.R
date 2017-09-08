# this function converts a SSI Web like experimental design into a dummy coded binary design

convertSSItoDesign <- function(df.in, no.output = FALSE, none.col = NULL, nlev = NULL) {
  if(is.null(nlev)) nlev <- apply(df.in, 2, max, na.rm = TRUE)
  all.mat <- NULL
  name.vec <- NULL
  for (i in 1:ncol(df.in)) {                 # iterate all cols (attributes)
    # convert attribute column to a binary matrix of what was shown
    #
    # first, create a matrix of the right size
    att.max <- nlev[i]              # assumes levels occur as 1..max
    attmat1 <- matrix(0, nrow = length(df.in[,i]), ncol = att.max)

    # replace the matching elements with 1 to signify attribute presence
    attmat1[cbind( (1:length(df.in[,i])), df.in[,i]) ] <- 1

    # and add those columns to the master design matrix
    all.mat <- cbind(all.mat, attmat1)

    # create matching column names
    att.names <- paste(rep("ATT", att.max), i, "-", 1:att.max, sep="")
    name.vec <- c(name.vec, att.names)
  }
  all.mat <- data.frame(all.mat)
  names(all.mat) <- name.vec
  return(all.mat)
}
