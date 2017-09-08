# this function reads a ValueDriver def-file and extracts to relevant information

VD.read_def <- function(file, nlev) {
  natt <- length(nlev)
  def <- scan(file, what="character", sep="\n", strip.white=TRUE)
  start_AttLev <- which(def == "[Utilities]") + 1
  end_AttLev <- which(def == "[PURCHASE INTENTION]") - 1

  att_List <- vector("list", length = natt)

  help <- cumsum(c(2, nlev + 1))
  help_start <- (help + 1)[-(natt + 1)]
  help_end <- (help - 1)[2:(natt + 1)]

  for (i in sequence(natt)) {
    names(att_List)[i] <- def[help[i]]
    att_List[[i]] <- def[help_start[i]:help_end[i]]
  }

  def_seg <- scan(file, what="character", sep="\n")

  seg_part <- c((which(def_seg == "[Segmente]") + 1):(which(def_seg == "[FIXED]") - 1))

  def_seg <- def_seg[seg_part]

  seg_ind_BIN <- unlist(lapply(strsplit(def_seg, " "), function(x) {x[1] == ""}))

  def_seg_lab <- def_seg[!seg_ind_BIN]

  help_seg_ind <- cbind(which(!seg_ind_BIN) + 1, c(which(!seg_ind_BIN)[-1] - 1, length(def_seg)))
  seg_lev_ind <- apply(help_seg_ind, 1, function(x) {seq(x[1], x[2])})
  help_list <- lapply(seg_lev_ind, function(x) {def_seg[x]})

  def_seg_lev <- lapply(help_list, function(x) {unlist(lapply(strsplit(x, " "), function(y) {paste(y[-1], collapse = " ")})) })
  names(def_seg_lev) <- def_seg_lab

  return(list(att_List = att_List, nlev = nlev, natt = natt, def_seg = def_seg_lev, nseg = length(def_seg_lab), file_in = file))
}
