\name{pos.complement}
\alias{pos.complement}
\title{Returns the complement of a pos matrix again as a pos matrix}
\usage{
  pos.complement(pos, is.sorted = FALSE, start = 1,
    end = NULL, keep.pos = FALSE, str = NULL)
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
