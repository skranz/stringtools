\name{str.replace.by.blocks}
\alias{str.replace.by.blocks}
\title{Replaces in str every occurence of pattern by replacement}
\usage{
\method{str}{replace.by.blocks}(str, pattern, replacement, blocks = NULL,
  sub.txt = "SUB", block.start, block.end, block.ignore = NULL,
  use.levels = NULL, fixed = TRUE, only.replace.smaller.than = NULL,
  only.replace.larger.than = NULL)
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

  \item{only.replace.smaller.than}{if not NULL only
  replaces matches whose number of characters is less or
  equal to only.replace.smaller.than}

  \item{only.replace.larger.than}{if not NULL only replaces
  matches whose number of characters is bigger or equal to
  only.replace.larger.than}
}
\value{
a string
}
\description{
Replaces in str every occurence of pattern by replacement
}

