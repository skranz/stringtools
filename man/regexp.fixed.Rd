\name{regexp.fixed}
\alias{regexp.fixed}
\title{strings will be treated as fixed constant in regex
e.g. transforms c("A*",".") into c("\QA*\E","\Q.\E")}
\usage{
  regexp.fixed(str, fixed = TRUE)
}
\arguments{
  \item{str}{a string vector}

  \item{fixed}{if FALSE just return str}
}
\value{
  transformed string vector of same size as str
}
\description{
  strings will be treated as fixed constant in regex e.g.
  transforms c("A*",".") into c("\QA*\E","\Q.\E")
}

