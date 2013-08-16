\name{pos.complement}
\alias{pos.complement}
\title{Returns the complement of a pos matrix again as a pos matrix}
\usage{
  pos.complement(pos, is.sorted = FALSE, end = 1000000L,
    keep.pos = FALSE)
}
\description{
  Returns the complement of a pos matrix again as a pos
  matrix
}

\examples{\dontrun{
  pos = matrix(c(
    2,2,
    5,8,
    10,10
  ),3,2,byrow=TRUE)
  pos.complement(pos)
  pos.complement(pos,keep.pos=TRUE)  
}}
