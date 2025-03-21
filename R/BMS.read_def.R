# this function reads a BMS Pricer def-file and extracts to relevant information

#' Read BMS Pricer definitions file
#'
#' Reads the BMS Pricer def file extracting the labels and prices
#'
#'
#' @param file A string value with the path to the DEF file to import.
#' @return A list including elements
#'   \item{brands}{A vector including the SKU labels}
#'   \item{prices}{A list including the prices per SKU}
#'   \item{segDef}{A list including the segment labels}
#' @author Maximilian Rausch - Maximilian.Rausch@@bms-net.de
#' @examples
#'
#' \dontrun{
#' VP.read_def(file = "data/TEST_FILE.def")
#' }
#'
#' @export BMS.read_def
BMS.read_def <- function(file) {
  defIN <- readLines(file, skipNul = TRUE)
  defIN <- gsub("\t", " ", defIN)
  # EXTRACT BRAND INFO !--------------------------------------------------------------------------------------------------
  start_brand <- which(defIN == "Brand") + 1
  end_brand <- which(defIN == "Price 1" | defIN == "Price1") - 1

  brands <- gsub('^ {1,}| *\\{.*', '', defIN[seq(start_brand, end_brand)])


  # PRCIE INFO !----------------------------------------------------------------------------------------------------------
  prices <- defIN[seq(which(defIN == "Price 1" | defIN == "Price1"),
                      ifelse(any(defIN == "[Fixed]"), which(defIN == "[Fixed]") - 1, which(defIN == "[Segmente]") - 1))]

  grep('Price', prices, value = TRUE)
  priceAtts_index <- c(grep('Price', prices), length(prices) + 1)

  priceList <- lapply(seq_along(priceAtts_index)[-1],
                      function(i) {
                        index <- (priceAtts_index[i - 1] + 1):(priceAtts_index[i] - 1)

                        priceOut <- regmatches(prices[index],
                                               regexpr('\\{(.*)\\}', prices[index]))

                        as.numeric(gsub('[\\{\\}]', '', priceOut))
                      })


  # SEGMENT INFO !--------------------------------------------------------------------------------------------------------

  seg_part <- c((which(defIN == "[Segmente]") + 1):(which(defIN == "[Variablen]") - 1))

  def_seg <- defIN[seg_part]

  seg_ind_BIN <- grepl('^ ', def_seg)

  def_lab <- def_seg[!seg_ind_BIN]

  segList <- lapply(seq_along(which(!seg_ind_BIN)),
                    function(i) {
                      # i <- 1
                      start_ind <- which(!seg_ind_BIN)[i] + 1
                      if (i == length(which(!seg_ind_BIN))) {
                        end_ind <- length(def_seg)
                      } else {
                        end_ind <- which(!seg_ind_BIN)[i + 1] - 1
                      }
                      index <- start_ind:end_ind

                      gsub('^ ', '', def_seg[index])
                    })
  names(segList) <- def_lab

  invisible(list(brands = brands, prices = priceList, segDef = segList))
}
