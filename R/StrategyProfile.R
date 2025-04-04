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
#' @param asdRules A list with asd rules
#'   \describe{
#'     \item{baseAtt}{base attribute for asd rule}
#'     \item{baseLev}{level of base attribute}
#'     \item{asdAtt}{asd attributes. Index values of th eattributes that are excluded with th ebase attribute}
#'     }
#' @param defaultLev A integer or vector with same nength as number of asd attributes with the default level
#'                   for asd attributes attribute.
#'                   Default value is first level for all if \code{NULL}.
#' @param SimModel A character value indicating the Simulation Model.
#'   \describe{
#'     \item{SoC}{Share of Choice - Preference Share (Default)}
#'     \item{FC}{First Choice}
#'     }
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
#'   scenInd = selConc,
#'   asdRules = asd_rules,
#'   defaultLev = selPrice,
#'   SimModel = "SoC")
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
    asdRules = NULL,
    defaultLev = NULL,
    SimModel = "SoC") {
  if (is.null(scenario)) stop("You need to provide a scenario!")
  if (is.null(att_List)) stop("You need to provide a att_List (e.g. from VD.read_def)!")
  if (is.null(utils_mat)) stop("You need to provide a utility matrix!")
  if (is.null(asdRules)) cat("no ASD rules provied.\n")
  if (length(scenInd) > 1) stop("You can only provide one scenario!")
  if (!(SimModel %in% c("SoC", "FC", "Preference"))) cat("Unclear Simulation Model. Simulation Model set to SoC.\n")

  nlev <- sapply(att_List, length)

  asdConds <- lapply(asdRules,
                     function(x) {
                       x$asdAtt
                     })

  asdAttsDom <- t(sapply(asdRules,
                         function(x) {
                           cbind(x$baseAtt,
                                 x$baseLev)
                         }))

  asdAttsCond <- sort(unique(unlist(asdConds)))

  if (is.null(defaultLev)) defaultLev <- 1

  if (length(defaultLev) > 1 &
      length(defaultLev) != length(asdAttsCond)) {
    cat("Length of asd rules and default values do not align.\n Values set to 1\n")
    defaultLev <- 1
  }

  if (any(nlev[asdAttsCond] < defaultLev)) stop("Default values out of bounds (defaultLev > nLev)!")

  asdDefault <- lapply(asdConds,
                       function(x) {
                         # x <- asdConds[[1]]
                         helpVec <- rep(as.numeric(NA),
                                        max(asdAttsCond))
                         helpVec[asdAttsCond] <- defaultLev
                         helpVec[x] <- 0
                         helpVec[!is.na(helpVec)]
                       })

  if (is.null(defaultLev)) {
    defaultLev <- 1
  }

  # simulate scenario shares
  Sim_Scen <- VD.computeShares(design = scenario,
                               utils = utils_mat,
                               nlev = c(nlev, 1),
                               weight = NULL,
                               FC = SimModel == "FC",
                               dummy = FALSE)

  Sim_DT <- data.table::data.table(Conc = paste0("Conc_", 1:length(Sim_Scen$meanShares)),
                                   SoC = round(Sim_Scen$meanShares, 3) * 100)

  # define helper for Strategy Profile scenarios
  stratHelp_List <- lapply(which(scenario[scenInd, ] != 0),
                           function(i) {
                             # i <- 1
                             scenList <- lapply(sequence(nlev[i]),
                                                function(j) {
                                                  # j <- 1
                                                  out <- scenario
                                                  out[scenInd, i] <- j

                                                  if (!is.null(asdRules)) {
                                                    if (i %in% asdAttsDom[, 1]) {
                                                      if (j %in% asdAttsDom[, 2]) {
                                                        if (scenario[scenInd, i] != j) {
                                                          out[scenInd, asdAttsCond] <- asdDefault[[j]]
                                                        }
                                                      }
                                                    }
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
                                                      FC = SimModel == "FC",
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
      data.table::data.table(dt - dt_base)
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
