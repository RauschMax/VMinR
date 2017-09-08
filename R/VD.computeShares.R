# function computes shares for a ValueDriver study - First Choice and Preference Share

VD.computeShares <- function(design, utils, nlev, weight = NULL, FC = FALSE, dummy = TRUE) {
  if (!dummy) dummy_design <- as.matrix(convertSSItoDesign(design, nlev = nlev))
  if (dummy) dummy_design <- design

  if (is.null(weight)) weight = rep(1, dim(utils)[1])

  xBeta <- t(dummy_design %*% t(utils))
  head(xBeta)

  exp_xbeta <- exp(xBeta)
  head(exp_xbeta)

  if (FC) {
    FC_func <- function(x) {
      fc <- (x == max(x)) * 1
      fc_scaled <- fc / sum(fc)
      return(fc_scaled)
    }

    probabilities <- t(apply(xBeta, 1, FC_func))
  }
  else {
    probabilities <- exp_xbeta / rowSums(exp_xbeta)
  }

  return(list(meanShares = apply(probabilities, 2, weighted.mean, w = weight),
              indShares = probabilities))
}
