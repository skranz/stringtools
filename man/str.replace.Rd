\name{str.replace}
\alias{str.replace}
\title{Replaces in str every occurence of pattern by replacement}
\usage{
  str.replace(str, pattern, replacement, fixed = TRUE,
    perl = FALSE, ignore = NULL, ignore.pos = NULL,
    pos.only = NULL, ignore.pattern = "_IGNORE_", ...)
}
\arguments{
  \item{str}{the string to replaced}

  \item{pattern}{the substring to be replaced}

  \item{replacment}{the new substrings}
}
\value{
  a string
}
\description{
  Replaces in str every occurence of pattern by replacement
}

\examples{\dontrun{
  str = c("12345678901234567890")
  pattern = c("34","12")
  replacement = c("AB","Holla die Waldfee")
  pos = cbind(1,10)
  str.replace(str,pattern,replacement, ignore.pos=pos)
  str.replace(str,pattern,replacement, pos.only=pos)
  str.replace(str,pattern,replacement)
  
  str = "int{5*2}*{2*3}"
  pattern = "int{_IGNORE_}"
  replacement = "integer{_IGNORE_}"  
  pos = cbind(c(5,11),c(7,13))
  str.replace(str,pattern,replacement, ignore.pos=pos)
}}
