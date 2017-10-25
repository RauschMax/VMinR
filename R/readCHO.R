# function reads a cho-file

readCHO <- function(fileIN, progress = TRUE) {

  cho_input <- utils::read.table(fileIN, fill=TRUE)

  ## relevant for Fortschrittsbalken ##
  count <- 0
  ##

  count1 <- 1
  ind_info <- NULL
  nconc <- NULL
  cho <- NULL
  design <- NULL

  repeat {

    # 1st line of each respondent; info on ID, nsegs, nattr, nlevels, none
    ind_info <- rbind(ind_info, cho_input[count1, which(!is.na(cho_input[count1,]))])

    # index vector for the line wich gives the number of concepts per task
    nconc_ind <- count1 + 1 + (ind_info[nrow(ind_info), 2] != 0)

    for (i in sequence(ind_info[nrow(ind_info), 4])[-1]) {

      nconc_ind <- c(nconc_ind, nconc_ind[i - 1] + cho_input[nconc_ind[i - 1], 1] + 2)

    }

    # read number of concepts per tasks using index vector
    nconc[[length(nconc) + 1]] <- cho_input[nconc_ind, 1]

    # index vector for the line wich gives the choice for each task
    cho_ind <- nconc_ind + nconc[[length(nconc)]] + 1

    # read choice for each task using index vector
    cho[[length(cho) + 1]] <- cho_input[cho_ind, 1]

    count0 <- count1
    count1 <- count1 + (ind_info[nrow(ind_info), 2] != 0) + sum(nconc[[length(nconc)]]) + 2 * length(nconc[[length(nconc)]]) + 1

    # index vector to read the design information (concepts shown per task)
    design_ind <- c(1:(count0 + (ind_info[nrow(ind_info), 2] != 0)), nconc_ind, cho_ind, count1:nrow(cho_input))

    # read the design information using the index vector
    design <- rbind(design, cho_input[-design_ind,])

    count <- count + 1
    if (progress) {
      ## Fortschrittsbalken ##
      if (count == 1) {

        max.iter <- ceiling(nrow(cho_input) / (count1 - count0))

        cat(paste(c("\r  |", rep.int(" ", 50), sprintf("| %3d%%", round(count/max.iter)*100)), collapse = ""))

      }

      if (count %% (round(max.iter/100, 0)) == 0) cat(paste(c("\r  |", rep.int("=", ceiling((count/max.iter)*50)), rep.int(" ", abs(50 - ceiling((count/max.iter)*50))), sprintf("| %3d%%", round((count/max.iter)*100))), collapse = ""))
      utils::flush.console()

      if(all(is.na(cho_input[count1,]))) {

        if (50 - ceiling((count/max.iter)*50) < 0) cat("\n number of concepts/tasks differ between respondents (e.g. ACBC) \n")

        break

      }
      ## Fortschrittsbalken - ENDE ##
    }

  }

  # delete NA-columns from design matrix
  design <- design[, apply(design, 2, function(x) {all(!is.na(x))})]

  # create ID vector
  ID <- ind_info[,1]

  # design including "sequential version number", "ID", "task", "concept"
  design_out <- cbind(rep(seq_along(ind_info[, 1]), unlist(lapply(nconc, sum))),
                      rep(ind_info[, 1], unlist(lapply(nconc, sum))),
                      rep(unlist(lapply(lapply(nconc, length), sequence)), unlist(nconc)),
                      unlist(lapply(unlist(nconc), sequence)),
                      design)

  colnames(design_out)[1:4] <- c("seq_version", "ID", "task", "concept")

  # information to pass from function
  invisible(list(fileIN = fileIN,
                 choIN = cho_input,
                 ind_info = ind_info,
                 nconc = nconc,
                 choice = cho,
                 design = design,
                 design_out = design_out,
                 ID = ID)
  )

}
