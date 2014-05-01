\name{str.split}
\alias{str.split}
\title{Splits string vectors}
\usage{
\method{str}{split}(str, pattern, first = FALSE, keep.match = FALSE, ...)
}
\arguments{
  \item{str}{a vector of strings}

  \item{pattern}{vector where splits take place}
}
\value{
A list with same length as str. Each list element i
contains the split substrings from str[i]
}
\description{
Splits string vectors
}

