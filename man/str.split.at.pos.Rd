\name{str.split.at.pos}
\alias{str.split.at.pos}
\title{Splits a single string str at positions specified by pos}
\usage{
\method{str}{split.at.pos}(str, pos, keep.pos = FALSE, compl = NULL,
  max.char = max(nchar(str)), pos.mat.like.list = FALSE)
}
\arguments{
  \item{str}{character vector that shall be splitted}

  \item{pos}{split positions can be vector: assuming an
  element of size 1 that specifies a single char at that
  positions n*2 matrix: first column left position, right
  column right position list of vectors or matrices,
  specifying different pos for different str}

  \item{keep.pos}{default=FALSE. If TRUE add the tokens
  that describe the split to the result otherwise remove
  them}
}
\value{
single return is length of pos (if vector) or NCOL(pos) if
pos is matrix
}
\description{
Splits a single string str at positions specified by pos
}

