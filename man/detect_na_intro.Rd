% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detect_na_intro.R
\name{detect_na_intro}
\alias{detect_na_intro}
\title{Check for coercion's NA introduction}
\usage{
detect_na_intro(x, FUN)
}
\arguments{
\item{x}{a vector.}

\item{FUN}{a function for type coercion.}
}
\value{
A logical vector indicating where \code{NA}s were introduced.
}
\description{
Detect where coercing a vector introduces \code{NA}s.
}
\examples{
x <-  c("5", "12", "zero", NA, "0")
check <- detect_na_intro(x, as.numeric)
data.frame(x, check)
}
