\name{str.locate.all}
\alias{str.locate.all}
\title{Finds start and end positions of all substrings that match pattern}
\usage{
\method{str}{locate.all}(str, pattern, fixed = TRUE, perl = FALSE,
  ignore = NULL, ignore.pos = NULL, only.pos = NULL)
}
\arguments{
  \item{ignore.pos}{a logical vector or logical matrix
  indicating which locations of str shall be ignored in the
  search}
}
\value{
a list, of matrices n * 2 matrices. The first column is the
start position, second column end position of each match
}
\description{
Finds start and end positions of all substrings that match
pattern
}

