\name{str.between}
\alias{str.between}
\title{Returns the between the first occurence of start and the first occurence of end}
\usage{
\method{str}{between}(str, start, end, ...)
}
\description{
Returns the between the first occurence of start and the
first occurence of end
}

\examples{\dontrun{
  str = c("a * (4+3)","b+3)+1","(a*3+(1+2))", ")")
  str.between(str,"(",")")
}}
