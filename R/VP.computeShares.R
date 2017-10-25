# function computes shares for a ValuePricer study - First Choice and Preference Share

VP.computeShares <- function(utils, prices, simPrices, simSKUs = NULL, nlev, weight = NULL, none = FALSE, iaw = NULL, FC = FALSE) {
  if(is.null(simSKUs)) ifelse(length(simPrices) == dim(prices)[1], simSKUs <- seq_along(simPrices), stop("We need to know the SKUs to be simulated please!"))

  if (is.null(weight)) weight = rep(1, dim(utils)[1])

  price_ind <- matrix(seq(1, dim(prices)[1]*dim(prices)[2]) + dim(prices)[1], nrow=dim(prices)[1], ncol=dim(prices)[2],byrow=TRUE)

  base_pr_lev <- rep(NA, dim(prices)[1])
  for (i in seq_along(simSKUs)){
    base_pr_lev[simSKUs[i]] <- stats::approx(prices[simSKUs[i],], sequence(dim(prices)[2]), xout=simPrices[simSKUs[i]])$y
  }


  base_design_close <- as.matrix(cbind(convertSSItoDesign(cbind(simSKUs, diag(nlev[1])[simSKUs,]*base_pr_lev[simSKUs]), nlev=nlev), 0))

  row_interpol <- which(base_pr_lev %% 1 != 0)

  base_design <- base_design_close
  for (i in row_interpol) {
    col_interpol <- which(base_design_close[i, ] != 0)[2]
    base_design[i, c(col_interpol, col_interpol+1)] <- c(1-(base_pr_lev %% 1)[i],  (base_pr_lev %% 1)[i])
  }
  base_design[row_interpol,]

  if (none) {
    base_design <- cbind(base_design, 0)
    base_design <- rbind(base_design, c(rep(0, dim(base_design)[2]-1), 1))
  }

  # Xbeta <- base_design %*% t(as.matrix(utils))
  # if (none){
  #   exp_xbeta <- exp(xbeta) * matrix(c(diag(design), 1), ncol=dim(xbeta)[2], nrow=dim(xbeta)[1], byrow = TRUE)
  # } else {
  #   exp_xbeta <- exp(xbeta) * matrix(diag(design), ncol=dim(xbeta)[2], nrow=dim(xbeta)[1], byrow = TRUE)
  # }

  Xbeta <- base_design %*% t(as.matrix(utils))
  exp_xbeta <- exp(Xbeta)

  # logit transformation o futility sums
  # sim <- exp_xbeta / matrix(rep(colSums(exp_xbeta), dim(Xbeta)[1]), ncol=dim(Xbeta)[2], byrow=TRUE)

  if (FC) {
    if (is.null(iaw)) iaw = 1
    sim <- apply(Xbeta * iaw, 2, function(x) {(x == max(x))}) * 1
  }
  else {
    # Share of Preference
    sim <- exp_xbeta / matrix(rep(colSums(exp_xbeta), dim(Xbeta)[1]), ncol=dim(Xbeta)[2], byrow=TRUE)
    if (!is.null(iaw)) {
      sim <- sim * iaw
      sim <- sim / rowSums(sim)
    }
  }

  invisible(list(ind_sim = sim,
                 Xbeta = Xbeta,
                 simPrices = simPrices,
                 simSKUs = simSKUs,
                 prices = prices,
                 iaw = iaw,
                 none = none,
                 weight = weight,
                 simShares = apply(as.matrix(sim), 1, stats::weighted.mean, w=weight)))
}
