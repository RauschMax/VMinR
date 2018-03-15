# this function reads a ValueDriver def-file and extracts to relevant information



#' Read ValuePricer definitions file
#'
#' Reads the ValuePricer def file extracting the labels and prices
#'
#'
#' @param file A string value with the path to the DEF file to import.
#' @return A list including elements \item{brands}{A vector including the SKU
#' labels} \item{prices}{A matrix (nSKUs x nPrices) including the prices per
#' SKU}
#' @author Maximilian Rausch - Maximilian.Rausch@@tns-infratest.com
#' @examples
#'
#' \dontrun{
#' VP.read_def(file = "data/TEST_FILE.def")
#' }
#'
#' @export VP.read_def
VP.read_def <- function(file) {
  # def <- scan(file, what="character", sep="\n", strip.white=TRUE, quiet = TRUE)
  def <- readLines(file, skipNul = TRUE)
  def <- def[def != ""]
  start_brand <- which(def == "Brand") + 1
  end_brand <- which(def == "Price 1" | def == "Price1") - 1

  nBrands <- end_brand - start_brand + 1
  # nPriceLev <- (which(def == "Price 2" | def == "Price2") - 1) - which(def == "Price 1" | def == "Price1")
  prices <- def[seq(which(def == "Price 1" | def == "Price1"),
                    ifelse(any(def == "[Fixed]"), which(def == "[Fixed]") - 1, which(def == "[Segmente]") - 1))]

  brands_help <- def[seq(start_brand, end_brand)]
  brands <- gsub('^ {1,}| *\\{.*', '', brands_help)

  price_help <- regmatches(prices[-which(prices %in% c(paste("Price", sequence(nBrands)),
                                                       paste0("Price", sequence(nBrands))))],
                           regexpr('\\{(.*)\\}', prices[-which(prices %in% c(paste("Price", sequence(nBrands)),
                                                                             paste0("Price", sequence(nBrands))))]))
  prices1 <- gsub('[\\{\\}]', '', price_help)
  price_mat <- matrix(as.numeric(prices1), nrow = nBrands, byrow = TRUE)
  unlink("prices.data")

  seg_part <- c((which(def == "[Segmente]") + 1):(which(def == "[Variablen]") - 1))

  def <- def[seg_part]

  seg_ind_BIN <- unlist(lapply(strsplit(def, " "), function(x) {x[1] == ""}))

  def_lab <- def[!seg_ind_BIN]

  help_seg_ind <- cbind(which(!seg_ind_BIN) + 1, c(which(!seg_ind_BIN)[-1] - 1, length(def)))
  seg_lev_ind <- apply(help_seg_ind, 1, function(x) {seq(x[1], x[2])})
  help_list <- lapply(seg_lev_ind, function(x) {def[x]})

  def_lev <- lapply(help_list, function(x) {
    unlist(lapply(strsplit(x, " "), function(y) {paste(y[-1], collapse = " ")}))
    })
  names(def_lev) <- def_lab

  return(list(brands = brands, prices = price_mat, def = def_lev, nseg = length(def_lab)))
}
