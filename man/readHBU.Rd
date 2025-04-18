% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readHBU.R
\name{readHBU}
\alias{readHBU}
\title{Read Sawtooth HBU file}
\usage{
readHBU(fileIN)
}
\arguments{
\item{fileIN}{A string with the file name to be imported incl. path (if
necessary)}
}
\value{
A list including elements
\item{natt}{An integer returning the number of attributes}
\item{none}{An integer indicating if none is present (1 = yes; 0 = no)}
\item{npar}{An integer returning the total number of parameters
estimated for each individual}
\item{nlev}{A vector containing the number of levels for each attribute}
\item{effects_mat}{An attribute-by-attribute matrix of ones and zeros
(or minus ones) which indicates which effects were estimated.
Main effects are indicated by non-zero values on the diagonal.
Interactions are indicated by non-zero values in other positions.
Linear variables are indicated by negative entries on the diagonal.}
\item{hbu_labels}{A vector containing labels, one for each parameter estimated.}
\item{utils}{A matrix: Respondent number; An average value of RLH, obtained by averaging the
root-likelihood values for each random draw of his/her parameter estimates; A value of zero
(for compatibility with other modules); The total number of parameter estimates per respondent;
A negative one if a "None" option is included, or a zero if not
(for compatibility with other modules). It is followed by the parameter values for that respondent,}
\item{ID}{A vector containing the IDs}
}
\description{
Reads the Sawtooth HBU file with HB utilities
}
\examples{

\dontrun{
hbu <- readHBU("example.hbu")
}

}
\author{
Maximilian Rausch - Maximilian.Rausch@tns-infratest.com
}
