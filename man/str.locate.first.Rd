\name{str.locate.first}
\alias{str.locate.first}
\title{Finds start and end positions of first substring that matches pattern}
\usage{
\method{str}{locate.first}(str, pattern, fixed = TRUE, perl = FALSE,
  ignore = NULL, ignore.pos = NULL, only.pos = NULL)
}
\arguments{
  \item{ignore.pos}{a logical vector or logical matrix
  indicating which locations of str shall be ignored in the
  search}
}
\value{
single.return is a 1*2 matrix. First column start position,
second column end position
}
\description{
Finds start and end positions of first substring that
matches pattern
}

\examples{\dontrun{
  
  
  str.locate.first("Hello","l")
  str.locate.first(c("Hello","What","lol"),"l")
  str.locate.first("Hello",c("l","e"))
  str.locate.first(c("Hello","What","lol"),c("l","g","o"))
  
  
  str = "Hello ernie!"
  ignore = rep(FALSE,max(nchar(str)))
  ignore[c(2:4)] = TRUE
  pos = str.locate.first(str,"e",ignore=ignore)
  pos
  str.split.at.pos(str,pos[,1],keep.pos=TRUE)
  
  ignore.pos = cbind(2,4)
  pos = str.locate.first(str,"e",ignore.pos=ignore.pos)
  
  pos
  str.split.at.pos(str,pos[,1],keep.pos=TRUE)
  
  
  str.detect(str,c("A","[a-z]*"),fixed=FALSE)
  
  str = c("Hello ernie","abcdefg","hello erna")
  pos = str.locate.first(str,"e",ignore=ignore)
  pos
  str.split.at.pos(str,pos,keep.pos=TRUE,pos.mat.like.list=TRUE)
  
  # Compare regular expression matching
  str = c("012ab0121","adch3b23","0123")
  regexpr("[ab]*",str)
  gregexpr("[ab]*",str)
  gregexpr("[ab]*",str,perl=TRUE)
  str_locate(str,c("b"))  
  str_locate(str,"[ab]*")
  str_locate_all(str,"[ab]*")
  
  
  str.locate.first(str,"[ab]*",fixed=FALSE)
  str.detect(str,"[ab]*",fixed=FALSE)  
}}
