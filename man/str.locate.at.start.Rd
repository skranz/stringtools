\name{str.locate.at.start}
\alias{str.locate.at.start}
\title{Locate a pattern at the start of strings}
\usage{
\method{str}{locate.at.start}(str, pattern, fixed = TRUE)
}
\description{
Locate a pattern at the start of strings
}

\examples{\dontrun{
  str.locate.at.start(c("0123456012","1230","012012","01bsf"),"012")
  str.locate.at.start("0123456",c("012","0","1"))
  str.locate.at.end(c("0123456012","1230","012","01bsf"),"012")
  
}}
