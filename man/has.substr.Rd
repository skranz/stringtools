\name{has.substr}
\alias{has.substr}
\title{Returns for every element of str whether there is a match with pattern
works similar than grepl}
\usage{
  has.substr(str, pattern, fixed = TRUE, perl = FALSE,
    ignore = NULL)
}
\description{
  Returns for every element of str whether there is a match
  with pattern works similar than grepl
}

\examples{\dontrun{
  str = c("12347382709")
  pattern = c("a","4","56","34","766","b")
  str.has.substr(str,pattern)  
}}
