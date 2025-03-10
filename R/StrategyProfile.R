# function computes the Strategy Profile for a Feature Conjoint study

#' Compute Strategy Profile
#'
#' Calculates the Strategy Profile for a Feature Conjoint study
#'
#'
#' @param scenario A matrix with the scenario for which the Strategy Profile should be calculated
#' @param att_List A list containing the attributes and levels, e.g. from \code{\link{VD.read_def}}
#' @param utils_mat A matrix containing the utilities
#' @param scenInd An integer that indicates the scenario for which the Strategy Profile should be calculated
#' @param brandAtt An integer indicating the brand attribute in case of brand specific prices
#' @param priceInd A vector with the index values for the brand specific price attributes.
#' If \code{NULL} the respective prices will be determined from the \code{att_List}
#' @param priceDefault A vector with the default price levels for each brand specific price attribute.
#' Default value is first level for all if \code{NULL}.
#' @return A list including elements
#'   \item{Sim_Scen}{results of \code{\link{VD.computeShares}}} for \code{scenario}
#'   \item{Sim_DT}{data.table with mean share of choice}
#'   \item{StrategyAbs}{share of choice for the Strategy Profile results}
#'   \item{StrategyRel}{share of choice difference of the Strategy Profile results to \code{scenario} results}
#'   \item{Strategy_DT}{data.table with Strategy Profile (absolute and delta) for simulated concept}
#' @author Maximilian Rausch - Maximilian.Rausch@@bms-net.de
#' @examples
#'
#' \dontrun{
#' # Scenario
#' utils_mat <- VMinR$VDdata$utils_mat
#' defIN <- VD.read_def(file = "data/TEST_timtim_5seg_2_gew_gew2.def",
#'                      nlev = c(4, 6, 4, 2, 2, 2, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5))
#'
#' scenario <- rbind(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
#'                   c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0),
#'                   c(1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0),
#'                   c(1, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0),
#'                   c(1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0),
#'                   c(1, 6, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0),
#'                   c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
#'
#' priceInd <- grep("^Price", names(defIN$att_List))
#' SP_Test <- StrategyProfile(
#'   scenario = scenario,
#'   att_List = defIN$att_List,
#'   utils_mat = utils_mat,
#'   scenInd = 1,
#'   brandAtt = 2,
#'   priceInd = grep("^Price", names(defIN$att_List)),
#'   priceDefault = rep(1, length(priceInd)))
#'
#' SP_Test
#' }
#'
#' @export StrategyProfile
StrategyProfile <- function(
    scenario,
    att_List,
    utils_mat,
    scenInd = 1,
    brandAtt = 1,
    priceInd = NULL,
    priceDefault = NULL) {
  if (is.null(scenario)) stop("You need to provide a scenario!")
  if (is.null(att_List)) stop("You need to provide a att_List (e.g. from VD.read_def)!")
  if (is.null(utils_mat)) stop("You need to provide a utility matrix!")
  if (is.null(is.null(priceInd) &
              length(grep("^Price",
                          names(att_List))) == 0)) stop("You need to provide index values for the price attribute!")

  if (is.null(priceInd)) {
    priceInd <- grep("^Price", names(att_List))
  }

  if (length(brandAtt) > 1) stop("You can only provide one brand attribute!")
  if (length(scenInd) > 1) stop("You can only provide one scenario!")


  if (is.null(priceDefault)) {
    priceDefault <- rep(1, length(priceInd))
  }

  nlev <- sapply(att_List, length)

  # simulate scenario shares
  Sim_Scen <- VD.computeShares(design = scenario,
                               utils = utils_mat,
                               nlev = c(nlev, 1),
                               weight = NULL,
                               FC = FALSE,
                               dummy = FALSE)

  Sim_DT <- data.table::data.table(Conc = paste0("Conc_", 1:length(Sim_Scen$meanShares)),
                                   SoC = round(Sim_Scen$meanShares, 3) * 100)

  # define helper for Strategy Profile scenarios
  stratHelp_List <- lapply(which(scenario[scenInd, ] != 0),
                           function(i) {
                             # i <- 2
                             scenList <- lapply(sequence(nlev[i]),
                                                function(j) {
                                                  # j <- 1
                                                  out <- scenario

                                                  pHelp <- out[scenInd, priceInd]
                                                  pLevHelp <- pHelp[pHelp != 0]

                                                  out[scenInd, i] <- j
                                                  if (i == brandAtt) {
                                                    out[scenInd, priceInd[which(pHelp == 1)]] <- 0
                                                    out[scenInd, priceInd[j]] <- ifelse(which(pHelp == 1) == j,
                                                                                        pLevHelp, priceDefault[j])
                                                  }
                                                  out
                                                })
                             names(scenList) <- att_List[[i]]
                             scenList
                           })
  names(stratHelp_List) <- names(att_List)[which(scenario[scenInd, ] != 0)]

  # absolute values for Strategy Profile
  StrategyAbs <- lapply(
    stratHelp_List,
    function(l1) {
      data.table::rbindlist(
        lapply(
          l1,
          function(scen) {
            data.table::data.table(t(VD.computeShares(design = scen,
                                          utils = utils_mat,
                                          nlev = c(nlev, 1),
                                          weight = NULL,
                                          FC = FALSE,
                                          dummy = FALSE)$meanShares))
          }))
    })

  # reference value for scenario
  helpSimSoC <- data.table::data.table(t(Sim_Scen$meanShares))

  # relative values for Strategy Profile
  StrategyRel <- lapply(
    StrategyAbs,
    function(dt) {
      dt_base <- helpSimSoC[rep(1, nrow(dt)), ]
      data.table(dt - dt_base)
    })

  attLabel_help <- att_List[which(scenario[scenInd, ] != 0)]
  nlev_help <- nlev[which(scenario[scenInd, ] != 0)]

  StrategyConcept <- data.table::data.table(Index = paste0("A", rep(seq_along(nlev_help), nlev_help),
                                                           "_", sequence(nlev_help)),
                                            Attribute = rep(names(attLabel_help), nlev_help),
                                            Level = unlist(attLabel_help),
                                            Absolute = unlist(lapply(StrategyAbs,
                                                                     function(dt) {

                                                                       dt[, scenInd, with = FALSE]
                                                                     })),
                                            Relative = unlist(lapply(StrategyRel,
                                                                     function(dt) {
                                                                       dt[, scenInd, with = FALSE]
                                                                     })))

  invisible(
    list(Sim_Scen = Sim_Scen,
         Sim_DT = Sim_DT,
         StrategyAbs = StrategyAbs,
         StrategyRel = StrategyRel,
         Strategy_DT = StrategyConcept))

}
