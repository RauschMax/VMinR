# this function reads a ValueDriver def-file and extracts to relevant information



#' Read ValueDriver definitions file
#'
#' Reads the ValueDriver def file containing the definition (e.g. labels,
#' prices)
#'
#'
#' @param file A string value with the path to the DEF file to import.
#' @param nlev A vector indicating the number of levels per attribute
#' @return A list including elements
#'   \item{att_List}{A list containing one element per attribute which contains a vector of the levels}
#'   \item{nlev}{A vector indicating the number of levels per attribute}
#'   \item{natt}{A variable returning the number of attributes}
#'   \item{none}{A boolean variable TRUE if none is included in def file}
#'   \item{def}{A list containg the labels of the segment data (variable names and levels)}
#'   \item{nseg}{A variable returning the number of segments}
#'   \item{file_in}{A string value with the path to the DEF file which was passed to the function.}
#' @author Maximilian Rausch - Maximilian.Rausch@@tns-infratest.com
#' @examples
#'
#' \dontrun{
#' VD.read_def(file = "data/TEST_timtim_5seg_2_gew_gew2.def",
#'             nlev = c(4, 6, 4, 2, 2, 2, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5))
#' }
#'
#' @export VD.read_def
VD.read_def <- function(file, nlev = NULL) {
  def <- readLines(file, skipNul = TRUE)
  def <- def[def != ""]

  defHelp <- def[2:(grep("^[[]", def)[2] - 1)]

  attInd <- c(grep("^[^ ]", defHelp), length(defHelp) + 1)
  levDef <- lapply(seq_along(attInd)[-1],
                   function(i) {
                     out <- defHelp[(attInd[i - 1] + 1):(attInd[i] - 1)]
                     out <- sub("^[ ]", "", out)
                     out <- sub(" [{].*[}]", "", out)
                   })

  if (is.null(nlev)) {
    nlev <- sapply(levDef, length)
  }

  none <- !("[" %in% unlist(strsplit(def[grep("^[[]FIXED[]]", def) + 1], "")))

  natt <- length(nlev)

  att_List <- levDef
  names(att_List) <- defHelp[grep("^[^ ]", defHelp)]

  # att_List <- vector("list", length = natt)
  #
  # help <- cumsum(c(2, nlev + 1))
  # help_start <- (help + 1)[-(natt + 1)]
  # help_end <- (help - 1)[2:(natt + 1)]
  #
  # for (i in sequence(natt)) {
  #   names(att_List)[i] <- def[help[i]]
  #   att_List[[i]] <- sub("^ {1,}", "", def[help_start[i]:help_end[i]])
  # }

  seg_part <- c((which(def == "[Segmente]") + 1):(which(def == "[FIXED]") - 1))

  def <- def[seg_part]

  seg_ind_BIN <- unlist(lapply(strsplit(def, " "), function(x) {x[1] == ""}))

  def_lab <- def[!seg_ind_BIN]

  help_seg_ind <- cbind(which(!seg_ind_BIN) + 1, c(which(!seg_ind_BIN)[-1] - 1, length(def)))
  seg_lev_ind <- apply(help_seg_ind, 1, function(x) {seq(x[1], x[2])})

  if (!is.list(seg_lev_ind)) seg_lev_ind <- as.list(data.frame(seg_lev_ind))

  help_list <- lapply(seg_lev_ind, function(x) {def[x]})

  def_lev <- lapply(help_list, function(x) {
    # out <- unlist(lapply(strsplit(x, " "),
    #                      function(y) {
    #                        paste(y[-1], collapse = " ")
    #                      }))
    out <- sub("^[ ]", "", x)
    out
  })
  names(def_lev) <- def_lab

  return(list(att_List = att_List,
              nlev = nlev,
              natt = natt,
              none = none,
              def = def_lev,
              nseg = length(def_lab),
              file_in = file))
}
