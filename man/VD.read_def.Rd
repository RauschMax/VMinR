\name{VD.read_def}
\alias{VD.read_def}
\title{Read ValueDriver definitions file}
\usage{
VD.read_def(file, nlev)
}
\description{
Reads the ValueDriver def file containing the definition (e.g. labels, prices)
}
\arguments{
\item{file}{A string value with the path to the DEF file to import.}

\item{nlev}{A vector indicating the number of levels per attribute}
}
\value{
A list including elements
\item{att_List}{A list containing one element per attribute which contains a vector of the levels}
\item{nlev}{A vector indicating the number of levels per attribute}
\item{natt}{A variable returning the number of attributes}
\item{def_seg}{A list containg the labels of the segment data (variable names and levels)}
\item{nseg}{A variable returning the number of segments}
\item{file_in}{A string value with the path to the DEF file which was passed to the function.}
}
\author{Maximilian Rausch}
\examples{
\dontrun{
VD.read_def(file = "data/TEST_timtim_5seg_2_gew_gew2.def",
            nlev = c(4, 6, 4, 2, 2, 2, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5))
}
}

