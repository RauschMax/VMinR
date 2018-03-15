# function reads a hbu-file



#' Read Sawtooth HBU file
#'
#' Reads the Sawtooth HBU file with HB utilities
#'
#'
#' @param fileIN A string with the file name to be imported incl. path (if
#' necessary)
#' @return A list including elements
#' \item{natt}{An integer returning the number of attributes}
#' \item{none}{An integer indicating if none is present (1 = yes; 0 = no)}
#' \item{npar}{An integer returning the total number of parameters
#' estimated for each individual}
#' \item{nlev}{A vector containing the number of levels for each attribute}
#' \item{effects_mat}{An attribute-by-attribute matrix of ones and zeros
#' (or minus ones) which indicates which effects were estimated.
#' Main effects are indicated by non-zero values on the diagonal.
#' Interactions are indicated by non-zero values in other positions.
#' Linear variables are indicated by negative entries on the diagonal.}
#' \item{hbu_labels}{A vector containing labels, one for each parameter estimated.}
#' \item{utils}{A matrix: Respondent number; An average value of RLH, obtained by averaging the
#' root-likelihood values for each random draw of his/her parameter estimates; A value of zero
#' (for compatibility with other modules); The total number of parameter estimates per respondent;
#' A negative one if a "None" option is included, or a zero if not
#' (for compatibility with other modules). It is followed by the parameter values for that respondent,}
#' \item{ID}{A vector containing the IDs}
#' @author Maximilian Rausch - Maximilian.Rausch@@tns-infratest.com
#' @examples
#'
#' \dontrun{
#' hbu <- readHBU("example.hbu")
#' }
#'
#' @export readHBU
readHBU <- function(fileIN) {


  hbu_HEAD <- scan(fileIN, quiet = TRUE, what = character(), nmax = 5)

  none <- as.integer(hbu_HEAD[2])
  npar <- as.integer(hbu_HEAD[3])

  nlev <- as.integer(strsplit(readLines(fileIN, n = 2)[2], " ")[[1]])

  natt <- length(nlev)

  effects_mat <- matrix(scan(fileIN, quiet = TRUE, skip = 2, nmax = natt * natt),
                        ncol = natt, nrow = natt)

  hbu_labels <- scan(fileIN, quiet = TRUE, sep = "\n",
                     skip = 2 + natt, nmax = npar, what = character())

  utils <- utils::read.table(fileIN, skip = 2 + natt + npar)
  colnames(utils) <- c("respnum", "RLH", "V3", "npar", "none", paste0("U", rep(1:natt, nlev), "_", sequence(nlev)))
  if (none == 1) colnames(utils)[npar + 5] <- "NONE"

  # information to pass from function
  invisible(list(natt = natt,
                 none = none,
                 npar = npar,
                 nlev = nlev,
                 effects_mat = effects_mat,
                 hbu_labels = hbu_labels,
                 utils = utils,
                 ID = utils[, 1])
  )

}
