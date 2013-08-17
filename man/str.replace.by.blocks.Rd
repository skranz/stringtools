\name{str.replace.by.blocks}
\alias{str.replace.by.blocks}
\title{Replaces in str every occurence of pattern by replacement}
\usage{
  str.replace.by.blocks(str, pattern, replacement,
    blocks = NULL, sub.txt = "SUB", block.start, block.end,
    block.ignore = NULL, use.levels = NULL, ...)
}
\arguments{
  \item{str}{the string to replaced}

  \item{pattern}{the substring to be replaced}

  \item{replacment}{the new substrings}

  \item{block}{a block retrieved from str.block.pos
  alternatively, you can provide block.start and block.end}

  \item{block.start}{string with which the blocks start,
  e.g. "("}

  \item{block.end}{string with which the blocks end, e.g.
  ")"}
}
\value{
  a string
}
\description{
  Replaces in str every occurence of pattern by replacement
}

