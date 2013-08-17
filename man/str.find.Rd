\name{str.find}
\alias{str.find}
\title{Find substring positions or matches}
\usage{
  str.find(str, pattern, fixed = TRUE, first = FALSE,
    all = !first, simplify = TRUE, matches = FALSE, ...)
}
\arguments{
  \item{str}{vector of strings that will be searched}

  \item{pattern}{a search pattern}

  \item{fixed}{if FALSE perform regular expression search}

  \item{first}{shall only the first element be returned}

  \item{all}{shall all elements be returned}

  \item{simplify}{try to simplify a list return}

  \item{matches}{if FALSE pos will returned, otherwise the
  extracted substrings}
}
\description{
  A general wrapper from str.locate.first, str.locate.all,
  str.extract.first, str.extract.all
}

