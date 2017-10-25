# this function reads a ValueDriver def-file and extracts to relevant information

VP.read_def <- function(file) {
  def <- scan(file, what="character", sep="\n", strip.white=T)
  start_brand <- which(def == "Brand") + 1
  end_brand <- which(def == "Price 1") - 1

  nBrands <- end_brand - start_brand + 1
  nPriceLev <- (which(def == "Price 2") - 1) - which(def == "Price 1")
  prices <- def[seq(which(def == "Price 1"), ifelse(any(def == "[Fixed]"), which(def == "[Fixed]")-1, which(def == "[Segmente]")-1))]

  brands_help <- def[seq(start_brand, end_brand)]
  cat(brands_help, file="brands.data", sep="\n")
  brands <- scan("brands.data", what="character", flush=TRUE, sep="{")
  unlink("brands.data")

  cat(prices[-which(prices %in% paste("Price", sequence(nBrands)))], file = "prices.data", sep = "\n")
  prices1 <- scan("prices.data", what=list("character", "numeric"), sep="{", comment.char = "}")[[2]]
  price_mat <- matrix(as.numeric(prices1), nrow=nBrands, byrow=TRUE)
  unlink("prices.data")

  return(list(brands = brands, prices = price_mat))
}
