% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NNLS_MF.R
\name{NNLS_MF}
\alias{NNLS_MF}
\title{Performs the non-negative matrix factorisation for given phytoplankton
pigments and pigment ratios, to attain an estimate of phytoplankton
class abundances.}
\usage{
NNLS_MF(Fn, S, S_weights = NULL)
}
\arguments{
\item{Fn}{Pigment to Chl a matrix}

\item{S}{Sample data matrix – a matrix of pigment samples}

\item{S_weights}{Weights for each column}
}
\value{
A list containing
\enumerate{
\item The F matrix (pigment: Chl \emph{a}) ratios
\item The root mean square error (RMSE)
\item The C matrix (class abundances for each group)
}
}
\description{
Performs the non-negative matrix factorisation for given phytoplankton
pigments and pigment ratios, to attain an estimate of phytoplankton
class abundances.
}
\examples{
MC <- Matrix_checks(Sm,Fm)
Snew <- MC$Snew
Fnew <- MC$Fnew
S_weights <- Bounded_weights(Snew, weight.upper.bound = 30)
NNLS_MF(Fnew, Snew, S_weights)

}
