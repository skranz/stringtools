\name{str.locate.all}
\alias{str.locate.all}
\title{Finds start and end positions of all substrings that match pattern}
\usage{
  str.locate.all(str, pattern, fixed = TRUE, perl = FALSE,
    ignore = NULL)
}
\arguments{
  \item{ignore.pos}{a logical vector or logical matrix
  indicating which locations of str shall be ignored in the
  search}
}
\value{
  a list, of matrices n * 2 matrices. The first column is
  the start position, second column end position of each
  match
}
\description{
  Finds start and end positions of all substrings that
  match pattern
}

\examples{\dontrun{
  str.locate.all("0120121","1")
  str.locate.all(c("0120121","abce","011"),"1")

  str = c("0120121","abce","011bb1")
  #str = c("0120121")
  ignore = rep(FALSE,max(nchar(str)))
  ignore[c(2:4)] = TRUE  
  str.locate.all(str,"1",ignore=ignore)

  str.locate.all(str,c("1","b","a"),ignore=ignore)
  
  str = c("0120121")
  str.locate.all(str,c("1","b","2"),ignore=ignore)
  
  # Compare regular expression matching
  str = c("012ab0121","adch3b23","0123")
  gregexpr("[ab]*",str)
  str_locate_all(str,"[ab]*")  
  str.locate.first(str,"[ab]*",fixed=FALSE)
  str.locate.all(str,"[ab]*",fixed=FALSE)
  str.locate.all(str,c("[ab]*","3+","0*"),fixed=FALSE)
  str.locate.first(str,c("[ab]*","2","0*"),fixed=FALSE)
  str.locate.all(str,"ab",fixed=FALSE)
  
  return(ret)
}}
