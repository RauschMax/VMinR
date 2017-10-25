\name{readCHO}
\alias{readCHO}
\title{Read Sawtooth CHO file}
\usage{
readCHO(fileIN, progress = TRUE)
}
\description{
Reads the Sawtooth CHO file
}
\arguments{
\item{fileIN}{A string with the file name to be imported incl. path (if necessary)}
\item{progress}{A boolean variable indicating if progress bar should be displayed - default \code{TRUE}; set to FALSE if less than 50 cases to read.}
}
\value{
A list including elements
\item{fileIN}{A string which returns the input file name}
\item{choIN}{A matrix containing the raw imported cho-file}
\item{ind_info}{A matrix (one line per respondent) containing the info of the first line per respondent of the Sawtooth cho-file}
\item{nconc}{A list (one list element per respondent) containing vectors (length: ntasks) of the numbers of concepts per task. (Can vary per task, e.g. ACBC)}
\item{choice}{A list (one list element per respondent) of vectors (length: ntasks) containing the choices per task.}
\item{design}{A matrix containing the plain experimental design; no version, task or concept information included}
\item{design_out}{A matrix: design to be used in e.g. writeCHO;
                  1st column: sequential version number;
                  2nd column: ID;
                  3rd column: task;
                  4th column: concept;
                  5th column ++: design}
\item{ID}{A vector containing the IDs}
}
\author{Maximilian Rausch - Maximilian.Rausch@tns-infratest.com}
\examples{
\dontrun{
choIN <- readCHO("example.cho")
}
}

