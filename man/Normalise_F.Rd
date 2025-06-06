% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Normalise_F.R
\name{Normalise_F}
\alias{Normalise_F}
\title{This function normalises each column in F to row sum}
\usage{
Normalise_F(Fmat)
}
\arguments{
\item{Fmat}{A matrix or data.frame with the last column containing non-zeros}
}
\value{
A list consisting of two components:
- a matrix of pigment ratios normalized to row sums
- a vector of row sums
}
\description{
This function normalises each column in F to row sum
}
\keyword{internal}
