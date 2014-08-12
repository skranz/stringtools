\name{pos.with.complement}
\alias{pos.with.complement}
\title{Returns a pos matrix combined with its complement. The matrix has an attribute "complement" which is a logical vector indicating whether a row in the matrix is the original matrix or a complement}
\usage{
pos.with.complement(pos, is.sorted = FALSE, end = NULL)
}
\description{
Returns a pos matrix combined with its complement. The
matrix has an attribute "complement" which is a logical
vector indicating whether a row in the matrix is the
original matrix or a complement
}

\examples{\dontrun{
  pos = matrix(c(
    2,2,
    5,8,
    10,10
  ),3,2,byrow=TRUE)
  pos.with.complement(pos)
}}
