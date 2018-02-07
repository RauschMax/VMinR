# function reads a dat-file



#' Read Sawtooth DAT file
#'
#' Reads the Sawtooth DAT file
#'
#'
#' @param inFILE A string with the file name to be imported incl. path (if
#' necessary)
#' @param exportCOMPLETES A boolean variable indicating if a dat file with the
#' COMPLETE cases should be exported. default \code{FALSE}
#' @param exportUNIQUE A boolean variable indicating if a dat file with the
#' UNIQUE cases should be exported. default \code{TRUE}
#' @param ID_var A string variable giving the variable name of the ID variable.
#' default = "r"
#' @param out_unique A string variable in case the outfile should be
#' specifically labeled. If \code{NULL} label is set to fileIN_UNIQUE.dat
#' @param out_COMP A string variable in case the outfile should be specifically
#' labeled. If \code{NULL} label is set to fileIN_Complete.dat
#' @param progress A boolean variable indicating if progress bar should be
#' displayed - default \code{TRUE}; set to FALSE if less than 50 cases to read.
#' @return A list including elements \item{inFILE}{A string which returns the
#' input file name} \item{dat_file}{A character-vector containing the raw
#' imported dat-file} \item{dat_table}{A data.frame containing the information
#' from the dat file} \item{dat_completes}{A character-vector containing the
#' raw dat-file including status = "terminate" only} \item{dat_unique}{A
#' character-vector containing the raw dat-file with unique and complete cases
#' only}
#' @author Maximilian Rausch - Maximilian.Rausch@@tns-infratest.com
#' @examples
#'
#' \dontrun{
#' dat_file <- readDAT("example.dat")
#' }
#'
#' @export readDAT
readDAT <- function(inFILE, exportCOMPLETES = FALSE, exportUNIQUE = TRUE, ID_var = "r", out_unique = NULL, out_COMP = NULL, progress = TRUE) {

  if (is.null(out_unique)) out_unique <- paste0(strsplit(inFILE, split = "[.]")[[1]][1], "_UNIQUE.dat")
  if (is.null(out_COMP)) out_COMP <- paste0(strsplit(inFILE, split = "[.]")[[1]][1], "_complete.dat")

  # read the dat-file as character vector
  dat_file <- scan(inFILE, what="character", sep="\n", strip.white=TRUE, blank.lines.skip = FALSE, quiet = TRUE)

  # extract the single cases
  ind_mat <- cbind(c(1, which(dat_file == "")[-length(which(dat_file == ""))] + 1),
                   c(which(dat_file == "")[-length(which(dat_file == ""))], length(dat_file)))

  # creat list with one element per case
  dat_list <- vector(mode = "list", length = nrow(ind_mat))

  for (i in 1:nrow(ind_mat)) {

    dat_list[[i]] <- dat_file[ind_mat[i, 1]:ind_mat[i, 2]]

  }

  ## relevant für Fortschrittsbalken ##
  count <- 0
  max.iter <- length(dat_list) * 2
  ##

  # extract the information/variables from the dat-file
  dat <- NULL

  # general information (1st line)
  for (j in 1:length(dat_list)) {

    help1 <- unlist(strsplit(dat_list[[j]][1], split = " "))
    help1 <- as.character(help1[help1 != ""])

    dat <- rbind(dat, help1)
    dat <- as.data.frame(dat)
    dat[,1:6] <- apply(dat[,1:6], 2, as.numeric)
    dat[,7] <- as.character(dat[,7])

    colnames(dat)[1:7] <- c("V1", "sys_internal", "V3", "V4", "V5", "V6", "status")

    rm(help1)
    count <- count + 1

    if (progress) {
      ## Fortschrittsbalken ##
      if (count == 1) {

        cat(paste(c("\r  |", rep.int(" ", 50), sprintf("| %3d%%", round(count/max.iter)*100)), collapse = ""))

      }
      if (count %% (round(max.iter/100, 0)) == 0) cat(paste(c("\r  |", rep.int("=", ceiling((count/max.iter)*50)), rep.int(" ", abs(50 - ceiling((count/max.iter)*50))), sprintf("| %3d%%", round((count/max.iter)*100))), collapse = ""))
      utils::flush.console()
      ########################
    }
  }

  # variables
  for (j in 1:length(dat_list)) {

    help2 <- lapply(dat_list[[j]], strsplit, split = ",")[-1]

    help3 <- matrix(NA, nrow = length(help2), ncol = 2)

    for (l in 1:length(help2)) {

      help3[l, 1] <- help2[[l]][[1]][1]
      help3[l, 2] <- help2[[l]][[1]][2]

    }

    help3 <- help3[!is.na(help3[, 1]),]

    for (k in 1:nrow(help3)) {
      dat[j, help3[k, 1]] <- help3[k, 2]
    }

    rm(help2, help3)
    count <- count + 1

    if (progress) {
      ## Fortschrittsbalken ##
      if (count %% (round(max.iter/100, 0)) == 0) cat(paste(c("\r  |", rep.int("=", ceiling((count/max.iter)*50)), rep.int(" ", abs(50 - ceiling((count/max.iter)*50))), sprintf("| %3d%%", round((count/max.iter)*100))), collapse = ""))
      utils::flush.console()
      ########################
    }

  }

  # convert numeric variables to numeric (read as character)
  for (i in 1:ncol(dat)) {

    if (!any(is.na(as.numeric(dat[!is.na(dat[,i]),i])))) {

      dat[,i] <- as.numeric(dat[,i])

    }

  }

  ind_complete <- unlist(apply(ind_mat[which(dat[,7] == "terminate"), ], 1, function(x) {seq(x[1], x[2])}))

  completes <- dat_file[ind_complete]

  if (exportCOMPLETES) {

    write(completes, file = out_COMP)

  }

  if (progress) {
    ## Fortschrittsbalken ##
    cat(paste(c("\r  |", rep.int("=", 50), rep.int(" ", 0), sprintf("| %3d%%", 100)), collapse = ""))
    utils::flush.console()
    ########################
  }

  if (!(ID_var %in% colnames(dat))) {
    cat("\n", ID_var, "not present in dat-file. \n ID was set to sys_internal!")
    ID_var <- "sys_internal"
  }

  unique_completes <- cbind(dat[,7], dat[, ID_var], NA)
  unique_completes[dat[,7] == "terminate", 3] <- duplicated(unique_completes[dat[,7] == "terminate", 2]) * 1

  dat$unique <- (as.numeric(unique_completes[,3]) == 0) * 1

  ind_unique <- unlist(apply(ind_mat[which(unique_completes[,3] == "0"), ], 1, function(x) {seq(x[1], x[2])}))

  unique <- dat_file[ind_unique]

  if (exportUNIQUE) {

    write(unique, file = out_unique)

  }

  cat("\n Warnings occured due to the conversion from character to numeric.")

  invisible(list(inFILE = inFILE,
                 dat_file = dat_file,
                 dat_table = dat,
                 dat_completes = completes,
                 dat_unique = unique

  ))

}
