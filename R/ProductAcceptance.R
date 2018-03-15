#' Calculate Product Acceptance (Buying Rate)
#'
#' Calculates the Productacceptance/Buying Rate for a given concept.
#'
#'
#' @param conc A binary vector containing 0s or 1s for each level of the model.
#' @param utils A matrix containing the utilities per respondent of the model.
#' Remove the NONE column if present.
#' @return A vector contaning the prodct acceptance/buying rate values of the concept for each respondent.
#' @author Maximilian Rausch - Maximilian.Rausch@@tns-infratest.com
#' @examples
#'
#' \dontrun{
#' ...
#' }
#'
#' @export prodAcceptance
prodAcceptance <- function(conc, utils) {

  unlist(lapply(utils %*% conc, function(x) exp(x) / (1 + exp(x))))

}
