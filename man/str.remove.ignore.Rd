\name{str.remove.ignore}
\alias{str.remove.ignore}
\title{ignore is a logical vector or matrix stating which char positions shall be ignored
the function removes the substrings for which ignore=TRUE}
\usage{
  str.remove.ignore(str, ignore)
}
\description{
  ignore is a logical vector or matrix stating which char
  positions shall be ignored the function removes the
  substrings for which ignore=TRUE
}

\examples{\dontrun{
  str =c("1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ","1234567890")
  ignore = rep(FALSE,max(nchar(str)))
  ignore[c(4:5,8:20)] = TRUE
  str
  str.remove.ignore(str,ignore)
}}
