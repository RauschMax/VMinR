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
  def <- scan(file, what="character", sep="\n", strip.white=T)
  start_brand <- which(def == "Brand") + 1
  end_brand <- which(def == "Price 1" | def == "Price1") - 1

  nBrands <- end_brand - start_brand + 1
  nPriceLev <- (which(def == "Price 2" | def == "Price2") - 1) - which(def == "Price 1" | def == "Price1")
  prices <- def[seq(which(def == "Price 1" | def == "Price1"),
                    ifelse(any(def == "[Fixed]"), which(def == "[Fixed]")-1, which(def == "[Segmente]")-1))]

  brands_help <- def[seq(start_brand, end_brand)]
  cat(brands_help, file="brands.data", sep="\n")
  brands <- scan("brands.data", what="character", flush=TRUE, sep="{")
  unlink("brands.data")

  cat(prices[-which(prices %in% paste("Price", sequence(nBrands)))], file = "prices.data", sep = "\n")
  prices1 <- scan("prices.data", what=list("character", "numeric"), sep="{", comment.char = "}")[[2]]
  price_mat <- matrix(as.numeric(prices1), nrow=nBrands, byrow=TRUE)
  unlink("prices.data")

  def_seg <- scan(file, what="character", sep="\n")

  seg_part <- c((which(def_seg == "[Segmente]") + 1):(which(def_seg == "[Variablen]") - 1))

  def_seg <- def_seg[seg_part]

  seg_ind_BIN <- unlist(lapply(strsplit(def_seg, " "), function(x) {x[1] == ""}))

  def_seg_lab <- def_seg[!seg_ind_BIN]

  help_seg_ind <- cbind(which(!seg_ind_BIN) + 1, c(which(!seg_ind_BIN)[-1] - 1, length(def_seg)))
  seg_lev_ind <- apply(help_seg_ind, 1, function(x) {seq(x[1], x[2])})
  help_list <- lapply(seg_lev_ind, function(x) {def_seg[x]})

  def_seg_lev <- lapply(help_list, function(x) {unlist(lapply(strsplit(x, " "), function(y) {paste(y[-1], collapse = " ")})) })
  names(def_seg_lev) <- def_seg_lab

  return(list(brands = brands, prices = price_mat, def_seg = def_seg_lev, nseg = length(def_seg_lab)))
}
