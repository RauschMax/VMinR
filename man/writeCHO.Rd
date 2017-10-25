\name{writeCHO}
\alias{writeCHO}
\title{Read Sawtooth CHO file}
\usage{
writeCHO(export_file = "outfile.cho", design_out,
         ind_info_OUT, nconc, cho, progress = TRUE)
}
\description{
writes the Sawtooth CHO file; Input is based on the list elements of \code{\link{readCHO}}.
}
\arguments{
\item{export_file}{A string with the file name to be writen to incl. path (if necessary)}
\item{design_out}{matrix/data.frame: design to be exported: 1st column: sequential version number; needs to be sequential 1 to nversions (no parts missing); 2nd column: ID; 3rd column: task; 4th column: concept; 5th column ++: design;}
\item{ind_info_OUT}{A matrix (one line per respondent) containing the info of the first line per respondent for the Sawtooth cho-file}
\item{nconc}{A list (one list element per respondent) of vectors (length: ntasks) containing the numbers of concepts per task. (Can vary per task, e.g. ACBC)}
\item{cho}{A list (one list element per respondent) of vectors (length: ntasks) containing the choices per task.}
\item{progress}{A boolean variable indicating if progress bar should be displayed - default \code{TRUE}; set to FALSE if less than 50 cases to read.}
}
\value{
No output returned. File writen to working directory.
}
\author{Maximilian Rausch - Maximilian.Rausch@tns-infratest.com}
\examples{
\dontrun{
writeCHO(export_file = "outfile.cho", choIN$design_out, choIN$ind_info_OUT, choIN$nconc, choIN$cho)
}
}

