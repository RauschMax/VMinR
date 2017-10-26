# function writes a cho-file



#' Read Sawtooth CHO file
#' 
#' writes the Sawtooth CHO file; Input is based on the list elements of
#' \code{\link{readCHO}}.
#' 
#' 
#' @param export_file A string with the file name to be writen to incl. path
#' (if necessary)
#' @param design_out matrix/data.frame: design to be exported: 1st column:
#' sequential version number; needs to be sequential 1 to nversions (no parts
#' missing); 2nd column: ID; 3rd column: task; 4th column: concept; 5th column
#' ++: design;
#' @param ind_info_OUT A matrix (one line per respondent) containing the info
#' of the first line per respondent for the Sawtooth cho-file
#' @param nconc A list (one list element per respondent) of vectors (length:
#' ntasks) containing the numbers of concepts per task. (Can vary per task,
#' e.g. ACBC)
#' @param cho A list (one list element per respondent) of vectors (length:
#' ntasks) containing the choices per task.
#' @param progress A boolean variable indicating if progress bar should be
#' displayed - default \code{TRUE}; set to FALSE if less than 50 cases to read.
#' @return No output returned. File writen to working directory.
#' @author Maximilian Rausch - Maximilian.Rausch@@tns-infratest.com
#' @examples
#' 
#' \dontrun{
#' writeCHO(export_file = "outfile.cho", choIN$design_out, choIN$ind_info_OUT, choIN$nconc, choIN$cho)
#' }
#' 
#' @export writeCHO
writeCHO <- function(export_file = "outfile.cho", design_out, ind_info_OUT, nconc, cho, progress = TRUE) {

  ## relevant für Fortschrittsbalken ##
  count <- 1
  max.iter <- length(cho)
  ##

  for (i in 1:length(cho)) {

    # info line: ID, #segments, #attributes, #tasks, none
    if (count == 1) {
      utils::write.table(ind_info_OUT[i, ], file = export_file, append = FALSE, col.names = FALSE, row.names = FALSE)
    } else {
      utils::write.table(ind_info_OUT[i, ], file = export_file, append = TRUE, col.names = FALSE, row.names = FALSE)
    }

    # segment line: additonal variables (usually not present/needed)
    # write(segments, file = export_file, append = TRUE) # unquote and define 'segments' in case #segments != 0

    # write each task per respondent
    for (j in 1:ind_info_OUT[i, 4]) {

      # #concepts
      write(c(nconc[[i]][j], 1), file = export_file, append = TRUE)

      # design of concept j
      utils::write.table(design_out[which(design_out[,1] == i & design_out[,3] == j), -(1:4)], file = export_file, append = TRUE, col.names = FALSE, row.names = FALSE)

      # choice, time
      write(c(cho[[i]][j], 99), file = export_file, append = TRUE)

    }

    if (progress) {
      ## Fortschrittsbalken ##
      if (count == 1) cat(paste(c("\r  |", rep.int(" ", 50), sprintf("| %3d%%", round(count/max.iter)*100)), collapse = ""))

      if (count %% (round(max.iter/100, 0)) == 0) cat(paste(c("\r  |", rep.int("=", ceiling((count/max.iter)*50)), rep.int(" ", 50 - ceiling((count/max.iter)*50)), sprintf("| %3d%%", round((count/max.iter)*100))), collapse = ""))
     utils:: flush.console()

      count <- count + 1
      ## Fortschrittsbalken - ENDE ##
    }

  }

}
