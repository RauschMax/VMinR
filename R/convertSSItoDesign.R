# this function converts a SSI Web like experimental design into a dummy coded binary design



#' Convert SSI like design file to dummy coding
#' 
#' Convert SSI like design file to dummy coding.
#' 
#' 
#' @param df.in A matrix/data.frame containg the design file to be recoded.
#' @param no.output not used
#' @param none.col not used
#' @param nlev A string value with the path to the DEF file to import.
#' @return A data.frame of the recoded dummy design
#' @author Maximilian Rausch - Maximilian.Rausch@@tns-infratest.com
#' @examples
#' 
#' nlev <- VMinR$VDdata$nlev
#' basecase <- VMinR$basecase
#' 
#' basecase_dummy_2 <- as.matrix(cbind(convertSSItoDesign(basecase, nlev = nlev), 0))
#' 
#' @export convertSSItoDesign
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
