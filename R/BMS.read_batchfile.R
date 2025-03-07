# this function reads a BMS Pricer batch import file and extracts relevant information

#' Read BMS Pricer definitions file
#'
#' Reads the BMS Pricer def file extracting the labels and prices
#'
#'
#' @param file A string value with the path to the XLSM batch file file to import.
#' @return A list including elements
#'   \item{scenarios}{A data.table including the scenarios}
#'   \item{prices}{A list including the prices per SKU}
#'   \item{clusters}{A data.table including the clusters defined in the batch file}
#'   \item{segments}{A list with the defined segments}
#'   \item{info}{A list with infos: Scenario labels, Clustering, Segment, Weighted}
#' @author Maximilian Rausch - Maximilian.Rausch@@bms-net.de
#' @examples
#'
#' \dontrun{
#' BMS.read_batchfile(file = "data/TEST_FILE.def")
#' }
#'
#' @export BMS.read_batchfile
BMS.read_batchfile <- function(file) {

  # Scenario Labels
  helpScenNames <- openxlsx::read.xlsx(file,
                                       sheetName = "Scenarios",
                                       rowIndex = 5,
                                       header = FALSE)
  scenNames <- unlist(helpScenNames[, -(1:2)])
  scenNames <- gsub("\n", " ", scenNames[!is.na(scenNames)])

  # Scenario Cluster
  helpScenClustering <- openxlsx::read.xlsx(file,
                                            sheetName = "Scenarios",
                                            rowIndex = 7,
                                            header = FALSE)
  scenCluster <- unlist(helpScenClustering[, -(1:2)])
  scenCluster <- scenCluster[!is.na(scenCluster)]

  # Scenario Segments
  helpScenSegment1 <- openxlsx::read.xlsx(file,
                                          sheetName = "Scenarios",
                                          rowIndex = 10,
                                          header = FALSE)
  scenSegment <- unlist(helpScenSegment1)
  scenSegment <- scenSegment[!is.na(scenSegment)]

  helpScenSegment2 <- openxlsx::read.xlsx(file,
                                          sheetName = "Scenarios",
                                          rowIndex = 12,
                                          header = FALSE)
  scenSegmentLevel <- unlist(helpScenSegment2[, -1])
  scenSegmentLevel <- scenSegmentLevel[!is.na(scenSegmentLevel)]

  # Scenario Weight
  helpScenWeight <- openxlsx::read.xlsx(file,
                                        sheetName = "Scenarios",
                                        rowIndex = 3,
                                        header = FALSE)
  scenWeight <- unlist(helpScenWeight)[2]

  # check
  if (!identical(length(scenNames),
                 length(scenCluster),
                 length(scenSegment),
                 length(scenSegmentLevel))) {
    stop("Scenario info is not identical in length!")
  }

  # info list
  info_list <- list(names = scenNames,
                    cluster = scenCluster,
                    segment = scenSegment,
                    segLevel = scenSegmentLevel,
                    weighted = scenWeight)

  # prices
  helpPrices <- openxlsx::read.xlsx(file,
                                    sheetName = "Products & Prices",
                                    startRow = 7,
                                    header = TRUE)
  # helpPrices <- helpPrices[, mget(grep("Price", names(helpPrices), value = TRUE))]
  helpPrices <- helpPrices[, grep("Price", names(helpPrices))]


  price_list <- lapply(1:nrow(helpPrices),
                       function(i) {
                         out <- unlist(helpPrices[i, grep("Price", names(helpPrices))])
                         out[!is.na(out)]
                       })
  price_list <- price_list[sapply(price_list, length) > 0]

  # cluster data.table
  cluster_DT <- data.table::data.table(openxlsx::read.xlsx(file,
                                                           sheetName = "Cluster",
                                                           startRow = 5,
                                                           header = TRUE))
  cluster_DT

  # segment list
  helpSegments <- openxlsx::read.xlsx(file,
                                      sheetName = "Segments",
                                      startRow = 5,
                                      header = TRUE)
  helpSegments <- data.frame(helpSegments[, grep("^[^N]", names(helpSegments))])

  segment_list <- lapply(helpSegments,
                         function(x) {
                           out <- x[!is.na(x)]
                           out[-1]
                         })
  names(segment_list) <- sapply(helpSegments,
                                function(x) {
                                  x[1]
                                })

  # scenarios
  scenario_DT <- openxlsx::read.xlsx(file,
                                     sheetName = "Scenarios",
                                     startRow = 14,
                                     header = TRUE)

  scenario_DT[, grep("Prices", names(scenario_DT))] <- sapply(scenario_DT[, grep("Prices",
                                                                                 names(scenario_DT))],
                                                              as.numeric)

  names(scenario_DT)[grep("Prices", names(scenario_DT))] <- c("BaseCase",
                                                              paste0("Scen",
                                                                     seq_along(grep("Prices",
                                                                                    names(scenario_DT))[-1])))
  scenario_DT <- data.table::data.table(scenario_DT)

  cat("warnings on NAs are ok due to empty fields in scenario definitions!",
      "check warnings() \n")

  invisible(list(scenarios = scenario_DT,
                 prices = price_list,
                 clusters = cluster_DT,
                 segments = segment_list,
                 info = info_list))
}
