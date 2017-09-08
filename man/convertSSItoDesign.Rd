\name{convertSSItoDesign}
\alias{convertSSItoDesign}
\title{Convert SSI like design file to dummy coding}
\usage{
convertSSItoDesign(df.in, no.output = FALSE, none.col = NULL, nlev = NULL)
}
\description{
Convert SSI like design file to dummy coding.
}
\arguments{
\item{df.in}{A matrix/data.frame containg the design file to be recoded.}

\item{no.output}{not used}

\item{none.col}{not used}

\item{nlev}{A string value with the path to the DEF file to import.}
}
\value{
A data.frame of the recoded dummy design
}
\author{Maximilian Rausch}
\examples{
nlev <- VMinR$VDdata$nlev
basecase <- VMinR$basecase

basecase_dummy_2 <- as.matrix(cbind(convertSSItoDesign(basecase, nlev = nlev), 0))
}

