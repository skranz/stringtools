\name{str.split.at.pos}
\alias{str.split.at.pos}
\title{Splits a single string str at positions specified by pos}
\usage{
  str.split.at.pos(str, pos, keep.pos = FALSE,
    compl = NULL, max.char = max(nchar(str)),
    pos.mat.like.list = FALSE)
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
  single return is length of pos (if vector) or NCOL(pos)
  if pos is matrix
}
\description{
  Splits a single string str at positions specified by pos
}

\examples{\dontrun{
  str = c("1234567890")
  pos = c(3,5,7)
  str.split.at.pos(str,pos,keep.pos = FALSE)
  str.split.at.pos(str,pos,keep.pos = TRUE)
  pos = rbind(c(2,3),c(5,5),c(7,9))
  str.split.at.pos(str,pos,keep.pos = FALSE)
  str.split.at.pos(str,pos,keep.pos = TRUE)
  
  # Multiple str
  str = c("Hello ernie","abcg","hello erna")
  pos = c(2,5,8)
  str.split.at.pos(str,pos,keep.pos=TRUE)
  pos = list(c(3,5),c(2),c(1,9))
  str.split.at.pos(str,pos,keep.pos=TRUE)
    
  str = c("Hello ernie","abcdefg","hello erna")
  pos = str.locate.first(str,"e",ignore=ignore)
  pos
  str.split.at.pos(str,pos,keep.pos=TRUE,pos.mat.like.list=FALSE)
  str.split.at.pos(str,pos,keep.pos=TRUE,pos.mat.like.list=TRUE)
  
}}
