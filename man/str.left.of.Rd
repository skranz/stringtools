\name{str.left.of}
\alias{str.left.of}
\title{Returns the substring left to the first occurence of pattern}
\usage{
\method{str}{left.of}(str, pattern, ..., not.found = str)
}
\description{
Returns the substring left to the first occurence of
pattern
}

\examples{\dontrun{
  str = c("a = 5","b+3","a+3 = 4 = 3", "=")
  str.left.of(str,"=")
  str.left.of(str,"=", not.found=NA)
  str.right.of(str,"=")
  str.right.of(str,"=", not.found=NA)  
}}
