\name{str.blocks.pos}
\alias{str.blocks.pos}
\title{Returns a pos matrix indicating blocks like brackets ( ) or quoted parts "text"}
\usage{
  str.blocks.pos(str, start, end, ignore = NULL,
    ignore.start = ignore, ignore.end = ignore,
    fixed = TRUE, fixed.start = fixed, fixed.end = fixed)
}
\description{
  We allow for nested blocks. The position matrix also has
  an attribute level that describes the level of each block
}

