\name{str.split}
\alias{str.split}
\title{Splits string vectors}
\usage{
  str.split(str, pattern, first = FALSE,
    keep.match = FALSE, ...)
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

\examples{\dontrun{
  str <- c("aes_afe_f", "qwe.rty", "yui0op[3", "b")
  #split x on the letter e
  str  
  str.split(str, "e", keep.match=TRUE)
  str.split(str, "e", first=TRUE, keep.match=TRUE)
  
  str = c("aes_afe_fe")
  ignore.pos = cbind(1,3)
  str.split(str, "e", keep.match=TRUE, ignore.pos=ignore.pos)
  str.split(str, "e", first=TRUE,keep.match=TRUE, ignore.pos=ignore.pos)
  
  
  str = "abscde3823nsd34"
  str.split(str, "[a-z]*", fixed=FALSE, keep.match=TRUE)
  str.split(str, c("[a-z]*","d"), fixed=FALSE, keep.match=TRUE)
  
  str = c("abscde3823nsd34","8748274")
  str.split(str, c("[a-z]*","d"), fixed=FALSE, keep.match=TRUE)
}}
