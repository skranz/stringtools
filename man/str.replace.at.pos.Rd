\name{str.replace.at.pos}
\alias{str.replace.at.pos}
\title{replace a string at the positions specified by pos}
\usage{
\method{str}{replace.at.pos}(str, pos, new, pos.mat.like.list = FALSE)
}
\arguments{
  \item{str}{a vector, or a single element}

  \item{pos}{a matrix of substring positions, or a list of
  such matrices if str is a vector}

  \item{new}{a vector of new strings for each substring
  position, or a list of such vectors if length(str)>1}
}
\value{
string (vector) of length(str) in which the substrings have
been replaced
}
\description{
replace a string at the positions specified by pos
}

