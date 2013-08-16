\name{str.remove.ends}
\alias{str.remove.ends}
\title{remove charcaters on left and right of a string
str.remove.ends(c("ABCDEF","01"),1,3) returns c("BC","")}
\usage{
  str.remove.ends(str, left = 0, right = 0)
}
\description{
  remove charcaters on left and right of a string
  str.remove.ends(c("ABCDEF","01"),1,3) returns c("BC","")
}

\examples{\dontrun{
  str.remove.ends(c("ABCDEF","01345"),1,3)
  str.remove.ends(c("ABCDEF"),1:2,1:2)  
  str.remove.ends(c("ABCDEF","01345"),1:2,1)
  # The following calls throw errors!
  str.remove.ends(c("ABCDEF","01345"),1:2,1:3)  
  str.remove.ends(c("ABCDEF","01345","NOW!"),1:2,1)
}}
