\name{merge.lines}
\alias{merge.lines}
\title{transforms c("A","B") into "A\nB"}
\usage{
  merge.lines(txt, collapse = "\n")
}
\description{
  transforms c("A","B") into "A\nB"
}

\examples{\dontrun{
  merge = merge.lines(c("A","B"))
  merge
  sep.lines(merge)
}}
