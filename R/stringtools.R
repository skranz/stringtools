# #' A package with functions for string and text modifications to complement stringr
# #' 
# #' @name stringtools-package
# #' @docType package
# #' @author Sebastian Kranz \email{sebkranz@@gmail.com}


#' pos can be a 
#'   vector: assuming an element of size 1 that specifies a single char at that positions
#'   n*2 matrix: first column left position, right column right position
#'   list of vectors or matrices, specifying different pos for different str

# Special charcters that can appear in find patterns

.onLoad = function(...)  {
  # If loaded as library overwrite restore.point to an empty function
  assign("restore.point", function(...){}, envir=parent.env(environment()))
}

glob = new.env()
glob$DO_CHECK_STR_PAR = TRUE

#' Check if parameter to a str function have allowed dimensions
check.str.par = function(str,para) {  
  if(!glob$DO_CHECK_STR_PAR)
    return
  if (length(para)>1) {
    len = sapply(para,length)
  } else {
    len = length(para)
  }
  len = c(length(str),len)
  if ((length(unique(len[len>1]))>1)) {
    stop("Lengths of parameters are ", paste(c("str",names(para)),"=",len,collapse=" ")," All parameters with length>1 must have the same length!")
  }
}


#' trims whitespaces from string
#' @export
str.trim = function(txt) {
  str_trim(txt)
}


#' strings will be treated as fixed constant in regex 
#' e.g. transforms c("A*",".") into c("\QA*\E","\Q.\E")
#'
#' @param str a string vector
#' @param fixed if FALSE just return str
#' @return transformed string vector of same size as str
#' @export
regexp.fixed =function(str,fixed=TRUE) {
  if (!fixed) return(str)
  paste("\\Q",str,"\\E",sep="")
}
examples.regexp.fixed = function() {
  str = c("A.","*")
  # regexp.fixed transforms strings
  regexp.fixed(str)
  # fixed in stringr flags strings instead
  fixed(str)
}

#' Transforms a vector of strings like c("A","B","C") into "A|B|C"
#'
#' @param str a string vector
#' @param fixed if TRUE treats str as constants in regexp
#' @return a single string
#' @export
str.list.to.regexp.or = function(str,fixed=TRUE) {
	if (fixed) {
		return(paste(regexp.fixed(str),collapse="|"))
	} else {
		return(paste(str,collapse="|"))
	}
}
examples.str.list.to.regexp.or = function(){
  greek=c("alpha","beta","gamma")	
  str.list.to.regexp.or(greek)
}


#' transforms c("A","B") into "A\nB"
#' 
#' @export
merge.lines = function(txt, collapse = "\n") {
	paste(txt,collapse=collapse)
}

#' transforms "A\nB" into c("A","B") 
#' @export
sep.lines = function(txt, collapse = "\n") {
	if (length(txt)>1)
		txt = merge.lines(txt,collapse)
	str.split(txt,collapse)[[1]]
}
examples.merge.lines = test.sep.lines = function() {
  merge = merge.lines(c("A","B"))
  merge
  sep.lines(merge)
}
			
#' Returns a logical vector with TRUE for every character of str that is in pos
#' @export
str.inpos = function(str,pos) {
  stopifnot(length(str) == 1)
  inpos = rep(FALSE,nchar(str))
  if (length(pos)==0) return(inpos)
  
  for (i in 1:NROW(pos)) {
    inpos[pos[i,1]:pos[i,2]]=TRUE
  }
  return(inpos)
}


#' a synonym for nchar
#' @export
str.len = function(str) {
  nchar(str)
}

#' remove charcaters on left and right of a string
#' str.remove.ends(c("ABCDEF","01"),1,3) returns c("BC","")
#' @export
str.remove.ends = function(str, left=0,right=0) {
  check.str.par(str,list(left=left,right=right))
	substring(str,left+1,nchar(str)-right)
}
examples.str.remove.ends = function(str, left=0,right=0) {
  str.remove.ends(c("ABCDEF","01345"),1,3)
  str.remove.ends(c("ABCDEF"),1:2,1:2)  
  str.remove.ends(c("ABCDEF","01345"),1:2,1)
  # The following calls throw errors!
  str.remove.ends(c("ABCDEF","01345"),1:2,1:3)  
  str.remove.ends(c("ABCDEF","01345","NOW!"),1:2,1)
}

#' Returns als elements of txt that begin with start
#' @export
str.starts.with = function(txt,start) {
  substring(txt,1,nchar(start))==start
} 

examples.str.starts.with = function() {
  str = c("Hi how are you", "hi", "now what", "Hi")
  str.starts.with(str,"Hi")
}

#' keeps characters on left
#' @export
str.left = function(str, len=1) {
  check.str.par(str,list(len=len))
  substring(str,1,len)
}

#' keeps characters on right
#' @export
str.right = function(str, len=1) {
  check.str.par(str,list(len=len))
  substring(str,nchar(str)-len+1,nchar(str))
}


#' Splits a single string str at positions specified by pos
#' @param str character vector that shall be splitted
#' @param pos split positions can be  
#'   vector: assuming an element of size 1 that specifies a single char at that positions
#'   n*2 matrix: first column left position, right column right position
#'   list of vectors or matrices, specifying different pos for different str

#' @param keep.pos default=FALSE. If TRUE add the tokens that describe the split to the result otherwise remove them
#' @return single return is length of pos (if vector) or NCOL(pos) if pos is matrix
#' @export
str.split.at.pos = function(str, pos, keep.pos = FALSE, compl=NULL, max.char = max(nchar(str)),pos.mat.like.list=FALSE) {
      
  restore.point("str.split.at.pos")
  
  
  if (is.list(pos)) {
    stopifnot(length(str)==length(pos))
    fun = function(i) 
      str.split.at.pos(str[i],pos[[i]],keep.pos=keep.pos)
    return(lapply(seq_along(str),fun))
  } 
  if (!is.matrix(pos)) {
    pos = cbind(pos,pos)
  }
  if (NROW(pos)==0)
    return(str)
  if (pos.mat.like.list) {
    stopifnot(length(str)==NROW(pos))
    fun = function(i) 
      str.split.at.pos(str[i],pos[i,,drop=FALSE],keep.pos=keep.pos)
    return(lapply(seq_along(str),fun))
    
  }
  

  if (is.null(compl)) {
    compl = pos.complement(pos,keep.pos=keep.pos)
  }
  if (length(str)>1) {
    fun = function(i) 
      str.split.at.pos(str[i],pos,keep.pos=keep.pos,compl=compl,max.char=max.char)
    return(t(sapply(seq_along(str),fun)))
  }
  
  if (compl[NROW(compl),1]>max.char)
    compl = compl[-NROW(compl),,drop=FALSE]
  ret = substring(str,compl[,1],compl[,2])
  ret[is.na(ret)]=""
  return(ret)  
}

examples.str.split.at.pos = function() {
  str = c("1234567890")
  pos = c(3,5,7)
  str.split.at.pos(str,pos,keep.pos = FALSE)
  str.split.at.pos(str,pos,keep.pos = TRUE)
  pos = rbind(c(2,3),c(5,5),c(7,9))
  str.split.at.pos(str,pos,keep.pos = FALSE)
  str.split.at.pos(str,pos,keep.pos = TRUE)
  
  # Multiple str
  str = c("Hello ernie","abcg","hello erna")
  pos = c(2,5,8)
  str.split.at.pos(str,pos,keep.pos=TRUE)
  pos = list(c(3,5),c(2),c(1,9))
  str.split.at.pos(str,pos,keep.pos=TRUE)
    
  str = c("Hello ernie","abcdefg","hello erna")
  pos = str.locate.first(str,"e",ignore=ignore)
  pos
  str.split.at.pos(str,pos,keep.pos=TRUE,pos.mat.like.list=FALSE)
  str.split.at.pos(str,pos,keep.pos=TRUE,pos.mat.like.list=TRUE)
  
}

#' converts a string into a vector of single characters
#' @export
to.char.vector = function(str,collapse="") {
  if (length(str)>1)
    str = paste(str,collapse=collapse)

  nc = nchar(str)
  ind = 1:nc
  substring(str,ind,ind)
}

#' converts into a vector of strings into a matrix of single characters
#' @export
to.char.matrix = function(str,drop=FALSE) {
  if (length(str)==1 & drop) {
    nc = nchar(str)
    ind = 1:nc
    substring(str,ind,ind)
  } else {
    nc = max(nchar(str))
    ind = rep(1:nc,each=NROW(str))
    matrix(substring(rep(str,times=nc),ind,ind),nrow=length(str))
  }
}

#' converts a matrix of of single chars in a vector of one string per row 
#' @export
char.matrix.to.str = function(mat,collapse="") {
  if (!is.matrix(mat))
    return(paste(mat,collapse=collapse))
  
  apply(mat,1,paste,collapse=collapse)
}

#' converts a vector of chars into a single string or multiple strings, broken by sep
#' @export
char.vector.to.str = function(vec,sep=NULL,collapse="") {
  str = paste(vec,collapse="")
  if (!is.null(sep)) {
    str = sep.lines(str,sep)
  }
  return(str)
}

examples.to.char.matrix = function() {
  str =c("Now that is a nice matrix","but short!")
  mat = to.char.matrix(str)
  mat
  char.matrix.to.str(mat)
  vec = to.char.vector(str,collapse="\n")
  vec
  char.vector.to.str(vec,collapse="\n")
}

#' ignore is a logical vector or matrix stating which char positions shall be ignored
#' the function removes the substrings for which ignore=TRUE
#' @export
str.remove.ignore = function(str,ignore) {
  restore.point("str.remove.ignore")
  
  mat = to.char.matrix(str)
  if (NCOL(mat)==0)
    return(str)
  if (length(str)>1 & !is.matrix(ignore))
    ignore = matrix(ignore,nrow=NROW(str),ncol=NCOL(mat),byrow=TRUE)
  if (NCOL(ignore)>NCOL(mat))
    ignore = ignore[,1:NCOL(mat)]
  if (NCOL(ignore)<NCOL(mat)) {
    warning("str.remove.ignore: ignore has fewer columns than number of chars of longest element in str. Fill up with ignore=FALSE")
    old.ignore = ignore
    ignore = matrix(FALSE,NROW(ignore),NCOL(mat))
    ignore[,1:NCOL(old.ignore)] = old.ignore
  }
  
  mat[ignore] = ""
  char.matrix.to.str(mat)
}
examples.str.remove.ignore = function() {
  str =c("1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ","1234567890")
  ignore = rep(FALSE,max(nchar(str)))
  ignore[c(4:5,8:20)] = TRUE
  str
  str.remove.ignore(str,ignore)
}

#' Returns for every element of str whether there is a match with pattern
#' works similar than grepl
#' @export
has.substr =  function(str, pattern, fixed=TRUE, perl=FALSE, ignore =NULL) {
  ret = str.locate.first(str,pattern,fixed=fixed,perl=perl,ignore=ignore)
  !is.na(ret[,1])
}

#' Just a synonym for has.substr
#' @export
str.detect = has.substr

examples.has.substr = function() {
  str = c("abcdefgh","12347382709")
  pattern = c("a")
  str.has.substr(str,pattern)  
}

#' Find substring positions or matches
#' 
#' A general wrapper from str.locate.first, str.locate.all, str.extract.first, str.extract.all 
#' 
#' @param str vector of strings that will be searched
#' @param pattern a search pattern
#' @param fixed if FALSE perform regular expression search
#' @param first shall only the first element be returned
#' @param all shall all elements be returned
#' @param simplify try to simplify a list return
#' @param matches if FALSE pos will returned, otherwise the extracted substrings
#' @export
str.find = function(str, pattern, fixed=TRUE, first=FALSE,all=!first, simplify = TRUE,matches=FALSE,...) {
  restore.point("str.find")
  
  if (length(pattern)==0) {
    warning(str.find("called without pattern"))
    stop()
    return(str)
  }
  
  # Return the found matches instead of the position
  if (matches) {
    if (fixed)
      pattern = fixed(pattern)
    if (all) {
      ret = str_extract_all(str,pattern,...)
    } else {
      ret = str_extract(str,pattern,...)
    }
    if (simplify) {
      if (length(str)==1) {			
        if (first) {
          if (is.na(ret[[1]]))
            return(character(0)) 
          return(as.vector(ret[[1]]))
        }
        if (NROW(ret[[1]])==0)
          return(character(0)) 
        return(ret[[1]])
      }
      if (first) {
        return(ret)
      }
    }
    return(ret)
  }
  
  # Return position of found strings
  if (fixed)
    pattern = fixed(pattern)
  if (all) {
    ret = str_locate_all(str,pattern,...)
  } else {
    ret = str_locate(str,pattern,...)
  }
  if (simplify) {
    if (length(str)==1) {			
      if (first) {
        if (is.na(ret[[1]]))
          return(matrix(NA,nrow=0,ncol=2)) 
        return(as.vector(ret[[1]]))
      }
      if (NROW(ret[[1]])==0)
        return(matrix(NA,nrow=0,ncol=2)) 
      return(ret[[1]])
    }
    if (first) {
      return(ret)
    }
  }
  return(ret)
}


#' Finds start and end positions of first substring that matches pattern
#' @param ignore.pos a logical vector or logical matrix indicating which locations of str shall be ignored in the search
#' @return single.return is a 1*2 matrix. First column start position, second column end position
#' @export
str.locate.first = function(str, pattern, fixed=TRUE, perl=FALSE, ignore =NULL, ignore.pos=NULL,only.pos=NULL) {
  restore.point("str.locate.first")

  #print(ignore.pos)
  ignore = get.ignore(ignore,ignore.pos,only.pos,str=str)
  
  # Return positions of found strings  
  if (is.null(ignore)) {
    if (fixed) {
      ret = str_locate(str,fixed(pattern))
      return(ret)
    }
    if (length(pattern)==1) {
      reg.list = gregexpr(pattern,str,perl=perl)
    } else {
      stopifnot(length(str)==length(pattern) | length(str)==1 | length(pattern)==1)
      str = rep(str,length.out=length(pattern))
      fun.gregexpr = function(i) 
        gregexpr(pattern[i],str[i],perl=perl)[[1]]
      reg.list = lapply(seq_along(pattern),fun.gregexpr)
    }
    
    fun = function(reg) {
      len=attr(reg,"match.length")
      ind = which(len>0)
      if (length(ind)==0)
        return(c(NA,NA))
      return(reg[ind[1]]+c(0,len[ind[1]]-1))
    }
    ret.mat = t(sapply(reg.list,fun))
    return(ret.mat)
  }
  
  # Some char positions can be ignored
  ret = adapt.ignore.and.get.ci(str,ignore)
  ignore = ret$ignore
  ci = ret$ci

  # Ignore matches that are in ignore.char.pos
  str.ig = str.remove.ignore(str,ignore)

  restore.point("str.locate.first.ignore")
  
  ret = str.locate.first(str.ig,pattern,fixed=fixed,perl=perl)

  # Add the cummulated sum of ignored chars
  if (is.matrix(ci)) {
    fun = function(i) {
      ci.shifted = ci[i,!ignore[i,]]
      as.numeric(ret[i,1] + ci.shifted[ret[i,1]])
    }
    left = sapply(1:NROW(ci),fun)
    ret = ret+left-ret[,1]
  } else {
    ret = ret + ci[!ignore][ret[,1]]
  }
  return(ret) 
}


examples.str.locate.first = function() {
  
  
  str.locate.first("Hello","l")
  str.locate.first(c("Hello","What","lol"),"l")
  str.locate.first("Hello",c("l","e"))
  str.locate.first(c("Hello","What","lol"),c("l","g","o"))
  
  
  str = "Hello ernie!"
  ignore = rep(FALSE,max(nchar(str)))
  ignore[c(2:4)] = TRUE
  pos = str.locate.first(str,"e",ignore=ignore)
  pos
  str.split.at.pos(str,pos[,1],keep.pos=TRUE)
  
  ignore.pos = cbind(2,4)
  pos = str.locate.first(str,"e",ignore.pos=ignore.pos)
  
  pos
  str.split.at.pos(str,pos[,1],keep.pos=TRUE)
  
  
  str.detect(str,c("A","[a-z]*"),fixed=FALSE)
  
  str = c("Hello ernie","abcdefg","hello erna")
  pos = str.locate.first(str,"e",ignore=ignore)
  pos
  str.split.at.pos(str,pos,keep.pos=TRUE,pos.mat.like.list=TRUE)
  
  # Compare regular expression matching
  str = c("012ab0121","adch3b23","0123")
  regexpr("[ab]*",str)
  gregexpr("[ab]*",str)
  gregexpr("[ab]*",str,perl=TRUE)
  str_locate(str,c("b"))  
  str_locate(str,"[ab]*")
  str_locate_all(str,"[ab]*")
  
  
  str.locate.first(str,"[ab]*",fixed=FALSE)
  str.detect(str,"[ab]*",fixed=FALSE)  
}

#' Locate a pattern at the start of strings
#' @export
str.locate.at.start = function(str, pattern, fixed=TRUE) {
  restore.point("str.locate.at.start")
  if (!fixed)
    stop("Not yet implemented...")
  len = max(length(str),length(pattern))

  num.char = nchar(pattern)
  start = substring(str,1,num.char)
  
  mat = matrix(NA,len,2)
  does.start = which(start == pattern)
  if (length(does.start)==0)
    return(mat)
  
  num.char = rep(num.char, length.out = len)
  
  mat[does.start,1] = 1
  mat[does.start,2] = num.char[does.start]

  return(mat)
}

examples.str.locate.at.start = function() {  
  str.locate.at.start(c("0123456012","1230","012012","01bsf"),"012")
  str.locate.at.start("0123456",c("012","0","1"))
  str.locate.at.end(c("0123456012","1230","012","01bsf"),"012")
  
}


#' Locate a pattern at the end of str
#' @export
str.locate.at.end = function(str, pattern, fixed=TRUE) {
  restore.point("str.locate.at.end")
  if (!fixed)
    stop("Not yet implemented...")
  len = max(length(str),length(pattern))
  
  num.char = nchar(pattern)
  start = substring(str,nchar(str)-num.char+1,nchar(str))
  
  mat = matrix(NA,len,2)
  does.start = which(start == pattern)
  if (length(does.start)==0)
    return(mat)
  
  num.char = rep(num.char, length.out = len)
  
  mat[does.start,1] = (nchar(str)-num.char+1)[does.start]
  mat[does.start,2] = nchar(str)[does.start]
  
  return(mat)
}

examples.str.locate.at.end = function() {  
  str.locate.at.end(c("0123456012","1230","012","01bsf"),"012")  
}

#' Check if str completely matches a pattern (not just a substring)
#' @export
str.matches.pattern = function(str,pattern,fixed=TRUE) {
  if (!fixed)
    stop("Not yet implemented...")
  return(str == pattern)
}

# A helper function
adapt.ignore.and.get.ci = function(str,ignore) {
  restore.point("adapt.ignore.and.get.ci")
  
  maxchar = max(nchar(str))
  if (!is.matrix(ignore) & length(ignore)<maxchar) {
    ignore.old = ignore
    ignore = rep(FALSE,maxchar)
    ignore[1:length(ignore.old)] = ignore.old
  } else if (is.matrix(ignore) & NCOL(ignore)<maxchar) {
    ignore.old = ignore
    ignore = matrix(FALSE,NROW(ci),maxchar)
    ignore[,1:NCOL(ignore.old)] = ignore.old
  }
  
  ci = cumsum.ignore(ignore)
  if (!is.matrix(ignore)) {
    ignore = matrix(ignore,NROW(str),NROW(ignore),byrow=TRUE)
  }
  if (!is.matrix(ci)) {
    ci = matrix(ci,NROW(ignore),NCOL(ignore),byrow=TRUE)
  }
  return(list(ignore=ignore,ci=ci))
}

#' Finds start and end positions of all substrings that match pattern 
#' @param ignore.pos a logical vector or logical matrix indicating which locations of str shall be ignored in the search
#' @return a list, of matrices n * 2 matrices. The first column is the start position, second column end position of each match
#' @export
str.locate.all = function(str, pattern, fixed=TRUE, perl=FALSE, ignore =NULL, ignore.pos=NULL,only.pos=NULL) {
  restore.point("str.locate.all")

  #print(ignore.pos)
  ignore = get.ignore(ignore,ignore.pos,only.pos,str=str)
  
  
  if (is.null(ignore)) {
    if (fixed) {
      ret = str_locate_all(str,fixed(pattern))
      return(ret)
    }
    
    if (length(pattern)==1) {
      reg.list = gregexpr(pattern,str,perl=perl)
    } else {
      stopifnot(length(str)==length(pattern) | length(str)==1 | length(pattern)==1)
      str = rep(str,length.out=length(pattern))
      fun.gregexpr = function(i) 
        gregexpr(pattern[i],str[i],perl=perl)[[1]]
      reg.list = lapply(seq_along(pattern),fun.gregexpr)
    }
    
    fun = function(reg) {
      len=attr(reg,"match.length")
      ind = which(len>0)
      if (length(ind)==0)
        return(matrix(NA,0,2))
      left = reg[ind]
      right = left + len[ind]-1
      mat = matrix(c(left,right),NROW(ind),2)
      return(mat)
    }
    ret.mat = lapply(reg.list,fun)
    return(ret.mat)
  }
  
  # Some char positions can be ignored
  # Adapt positions to true positions
  if (length(str)==1 & length(pattern)>0)
    str = rep(str,length(pattern))
  if (length(str)==1 & is.matrix(ignore))
    str = rep(str,NROW(ignore))
  
  ret = adapt.ignore.and.get.ci(str,ignore)
  ignore = ret$ignore
  ci = ret$ci
  
  # Ignore matches that are in ignore.char.pos
  str.ig = str.remove.ignore(str,ignore)
  
  pos.list = str_locate_all(str.ig,pattern)
  
  add.ci.to.pos = function(i) {
    pos.mat = pos.list[[i]]
    if (length(pos.mat)==0)
      return(pos.mat)
    ci.shifted = ci[i,!ignore[i,]]
    ci.shifted = matrix(ci.shifted,NROW(pos.mat),length(ci.shifted),byrow=TRUE)
    left = as.numeric(pos.mat[,1] + ci.shifted[,pos.mat[,1]])
    pos.mat + left - pos.mat[,1]
  }
  lapply(seq_along(pos.list),add.ci.to.pos)
}

examples.str.locate.all = function() {  
  str.locate.all("0120121","1")
  str.locate.all(c("0120121","abce","011"),"1")

  str = c("0120121","abce","011bb1")
  #str = c("0120121")
  ignore = rep(FALSE,max(nchar(str)))
  ignore[c(2:4)] = TRUE  
  str.locate.all(str,"1",ignore=ignore)
  ignore.pos = rbind(c(2,4))
  str.locate.all(str,"1",ignore.pos=ignore.pos)
  
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
}

#' pos is a matrix or a list of matrices specifying positions as returned by str.locate.all
#' @export
str.at.pos = function(str,pos) {
  if (is.list(pos)) {
    restore.point("str.at.pos.list")
    
    stopifnot(length(pos)==length(str))
    
    fun = function(i)
      str.at.pos(str[i],pos[[i]])
    return(lapply(seq_along(str),fun))
  }
  restore.point("str.at.pos.no.list")
  #rerestore.point("str.at.pos.no.list")
  
  if (length(pos)==0)
    return(rep("",0))
  
  substring(str,pos[,1],pos[,2])
}

examples.str.at.pos = function() {  
  str = c("012ab0121","abce","0had112bb1")
  pos = str.locate.all(str,"[a-z]*",fixed=FALSE)
  pos
  str.at.pos(str,pos)
  return(ret)
}

#' Returns a list that contains for each element of str  (or pattern) a vector of all substrings that match the pattern. If for a string no element is matched an empty list is returned
#' @export
str.extract.all = function(str, pattern, fixed=FALSE, perl=FALSE, ignore =NULL) {
  pos = str.locate.all(str=str,pattern=pattern,fixed=fixed,perl=perl,ignore=ignore)
  return(str.at.pos(str,pos))
}

#' Returns a vector that contains for each element of str (or pattern) the first substring that matches pattern or NA if no match could be found
#' @export
str.extract.first = function(str, pattern, fixed=FALSE, perl=FALSE, ignore =NULL) {
  pos = str.locate.first(str=str,pattern=pattern,fixed=fixed,perl=perl,ignore=ignore)
  return(str.at.pos(str,pos))
}

examples.extract.all = function() {  
  str = "12ab12ab"
  regexec("(([0-9]+)([a-z]+))*",str)
  regexec("1",str)
  regexpr("([0-9]+)([a-z]+)",str)
  
  x <- c("A and B", "A, B and C", "A, B, C and D", "foobar")
  pattern <- "[[:space:]]*(,|and)[[:space:]]"
  ## Match data from regexpr()
  m <- regexpr(pattern, x)
  m
  
  regmatches(x, m)
  regmatches(x, m, invert = TRUE)
  
  ## Match data from gregexpr()
  m <- gregexpr(pattern, x)
  regmatches(x, m)
  regmatches(x, m, invert = TRUE)
  
  str.extract.first(c("0120121","abce","011"),"1")
  str.extract.all(c("0120121","abce","011"),"1")
    
  # Compare regular expression matching
  str = c("012ab0121","adch3b23","0123")
  str_extract_all(str,"[ab]*")  
  str.extract.all(str,"[ab]*")
  
  str_extract(str,"[ab]*")  
  str.extract.first(str,"[ab]*")
  
  return(ret)
}

#' Returns the number of matches of pattern in each element of str
str.number.matches = function(str, pattern,...) {
  res = str.locate.all(str,pattern,...)
  sapply(res,NROW)  
}

#' An alternative interface to str.split
#' @export
str.tokenize = function(str,split=" ",only.one.split=FALSE,simplify=TRUE,...) {
  ret = str.split(str,split,first=only.one.split,...)
  if (simplify & is.list(ret))
    ret = unlist(ret)
  return(ret)
}

#' Splits string vectors
#' @param str a vector of strings
#' @param pattern vector where splits take place
#' @return A list with same length as str. Each list element i contains the split substrings from str[i]
#' @export
str.split = function(str,pattern, first=FALSE, keep.match = FALSE,...) {
	restore.point("str.split")
	#rerestore.point("str.split")
  
	check.str.par(str,list(pattern=pattern))
	
	stopifnot(length(str)==length(pattern) | length(str)==1 | length(pattern)==1)
	if (length(str)==1)
	  str = rep(str,length.out=length(pattern))
	
  if (first) {
    pos = str.locate.first(str=str,pattern=pattern,...)
    return(str.split.at.pos(str,pos,keep.pos=keep.match))    
  } else {
    pos = str.locate.all(str=str,pattern=pattern,...)
    restore.point("jhhshf")
    return(str.split.at.pos(str,pos,keep.pos=keep.match))
  }
}
                    
examples.str.split = function() {
  str <- c("aes_afe_f", "qwe.rty", "yui0op[3", "b")
  #split x on the letter e
  str  
  str.split(str, "e", keep.match=TRUE)
  str.split(str, "e", first=TRUE, keep.match=TRUE)
  
  str = c("aes_afe_fe")
  ignore.pos = cbind(1,3)
  str.split(str, "e", keep.match=TRUE, ignore.pos=ignore.pos)
  str.split(str, "e", first=TRUE,keep.match=TRUE, ignore.pos=ignore.pos)
  
  
  str = "abscde3823nsd34"
  str.split(str, "[a-z]*", fixed=FALSE, keep.match=TRUE)
  str.split(str, c("[a-z]*","d"), fixed=FALSE, keep.match=TRUE)
  
  str = c("abscde3823nsd34","8748274")
  str.split(str, c("[a-z]*","d"), fixed=FALSE, keep.match=TRUE)
}


#' replace a string at the positions specified by pos
#' @param str a vector, or a single element
#' @param pos a matrix of substring positions, or a list of such matrices if str is a vector
#' @param new a vector of new strings for each substring position, or a list of such vectors if length(str)>1
#' @return string (vector) of length(str) in which the substrings have been replaced 
#' @export
str.replace.at.pos = function(str,pos,new,pos.mat.like.list=FALSE) {
	restore.point("str.replace.at.pos")
	
	if (is.list(pos)) {
	  stopifnot(length(str)==length(pos) & is.list(new) & length(new) == length(pos))
	  fun = function(i) 
	    str.replace.at.pos(str[i],pos[[i]],new[[i]])
	  return(lapply(seq_along(str),fun))
	} 
	if (!is.matrix(pos)) {
	  pos = cbind(pos,pos)
	}
	
	if (pos.mat.like.list) {
	  stopifnot(length(str)==NROW(pos))
	  fun = function(i) 
	    str.replace.at.pos(str[i],pos[i,,drop=FALSE],new[i])
	  return(lapply(seq_along(str),fun))
	  
	}
	
  if (length(str)>1) {
    stopifnot(length(str)==NROW(pos))
    fun = function(i) 
      str.replace.at.pos(str[i],pos,new)
    return(sapply(seq_along(str),fun))    
  }
  
  if (NROW(new)==0) return(str)
	
	if (NROW(pos)>1) {
		ord = order(pos[,1])
		pos = pos[ord,]
		new = new[ord]
	} else {
		if (pos[1,1]==1 & pos[1,2]==nchar(str))
			return(new)
	}
	
	# Every second element will be the new one
  pos.keep = pos.complement(pos,is.sorted=TRUE,end=nchar(str))
  str.keep = str.at.pos(str,pos.keep)
  all.pos = rbind(pos.keep,pos)
	ord = order(all.pos[,1])
	
  all.str = c(str.keep,new)[ord]
  return(paste(all.str,collapse=""))
}

examples.str.replace.at.pos = function() {
  str = "1234567890"
  pos = rbind(c(7,7),c(4,5))
  new = c("XXX","...")
  str.replace.at.pos(str,pos,new)
  
  str = c("1234567890","ahgdasdajsdgadhsabd")
  str.replace.at.pos(str,pos,new)          
}


examples.has.substr = function() {
  str = c("12347382709")
  pattern = c("a","4","56","34","766","b")
  has.substr(str,pattern)  
}

#' Replaces in str every occurence of pattern by replacement
#' 
#' @param str the string to replaced
#' @param pattern the substring to be replaced
#' @param replacment the new substrings
#' @return a string
#' @export
str.replace = function(str,pattern,replacement,fixed=TRUE,perl=FALSE,ignore=NULL, ignore.pos=NULL, only.pos=NULL,ignore.pattern="_IGNORE_",...) {
  #restore.point("str.replace")
  len = max(length(str),length(pattern),length(replacement)) 
  if (len > 1) {
    ret = sapply(1:len, function (i,...) {
      str.replace(str[min(length(str),i)],
                  pattern[min(length(pattern),i)],
                  replacement[min(length(replacement),i)],
                  fixed, perl,ignore,ignore.pos,only.pos,...)
    },...)
    return(ret)
  }  
  restore.point("str.replace.single")
  
  pos = ignore.and.complement.pos(ignore,ignore.pos,only.pos)  
  is.ignore = attr(pos,"is.ignore")
  if (sum(is.ignore)>0) {
    if (has.substr(pattern,ignore.pattern)) {        
      ig.pos=pos[is.ignore,,drop=FALSE]
      
      repl.pos= matrix(NA,NROW(ig.pos),2)
      new.str = vector("character", NROW(ig.pos))
      i = 2
      for (i in 1:NROW(ig.pos)) {
        # Replace ignored area i with placeholder ignore.pattern
        str.pl = str.replace.at.pos(str, ig.pos[i,,drop=FALSE], ignore.pattern)
        # Search for pattern in replaced string: get position and string
        rpos = str.locate.first(str.pl,pattern,fixed,perl, ignore.pos = ig.pos[-i,,drop=FALSE])
        ostr = str.at.pos(str.pl,rpos)
        rpos[,2] = rpos[,2]-nchar(ignore.pattern)+diff(ig.pos[i,])+1
        # Replace the string
        nstr = sub(pattern, replacement,ostr,fixed=fixed,perl=perl)
        nstr = sub(ignore.pattern,substring(str,ig.pos[i,1],ig.pos[i,2]),nstr,fixed=TRUE)
        #nstr = sub(pattern, replacement,ostr,fixed=fixed,perl=perl,...)
        
        repl.pos[i,] = rpos
        new.str[i] = nstr 
      }
      rem = duplicated(repl.pos) | is.na(repl.pos[,1])
      repl.pos = repl.pos[!rem,,drop=FALSE]
      new.str = new.str[!rem]
      mod.str = str.replace.at.pos(str, repl.pos,new.str)
      return(mod.str)
    } else {
      # Can simply search over the separate not ignored substrings
      sub = str.at.pos(str,pos)
      not.ignore = !attr(pos,"is.ignore")
      #ret = gsub(pattern, replacement,sub,fixed=fixed)
      ret = gsub(pattern, replacement,sub[not.ignore],fixed=fixed,perl=perl,...)
      sub[not.ignore] = ret
      return(paste0(sub,collapse=""))        
    }
  } else {
    return(gsub(pattern, replacement,str,fixed=fixed,...))
  }
}

examples.str.replace = function() {
  str = c("12345678901234567890")
  pattern = c("34","12")
  replacement = c("AB","Holla die Waldfee")
  pos = cbind(1,10)
  str.replace(str,pattern,replacement, ignore.pos=pos)
  str.replace(str,pattern,replacement, only.pos=pos)
  str.replace(str,pattern,replacement)
  
  str = "int{5*2}*{2*3}"
  pattern = "int{_IGNORE_}"
  replacement = "integer{_IGNORE_}"  
  pos = cbind(c(5,11),c(7,13))
  str.replace(str,pattern,replacement, ignore.pos=pos)
}

#' Performs sequentially all replacements of pattern and replace on the same strings str
#' 
#' A very slow implementation
#' @export
str.replace.list = function(str,pattern,replacement,...) {
  restore.point("str.replace.list")
  for (i in 1:NROW(pattern)) {
    str = str.replace(str,pattern[i],replacement[i],...)
  }
  return(str)
#   did.collapse = FALSE
#   if (NROW(str)>1) {
#     did.collapse = TRUE
#     str = paste(str,collapse=collapse)
#   }
#   pattern = regexp.or(pattern)
#   pos = str.find(str,pattern,fixed=FALSE,simplify=TRUE)
#   matches = str.substr(str,pos[,1],pos[,2])
#   match.ind = match(matches,pattern)
#   str = str.replace.at.pos(str,pos,pattern[match.ind])
#   if (did.collapse)
#     str = sep.lines(str,collapse)
#   if (!return.matches) {
#     return(str)
#   } else {
#     return(list(str=str,matches = matches, replaces=pattern[match.ind]))
#   }
}

examples.str.replace.list = function() {
  str.replace.list("na dies ist doch",c("a","e"),c("A","E"))
}

show.blocks = function(blocks, str) {
  data.frame(lev=blocks$levels, out.l=blocks$outer[,1], out.r = blocks$outer[,2],
                  in.l=blocks$inner[,1],in.r = blocks$inner[,2], str = substring(str,blocks$outer[,1],blocks$outer[,2]) )
}

show.pos = function(pos,str) {
  if (NROW(pos)==0)
    return(pos)
  data.frame(left=pos[,1],right=pos[,2], str = substring(str,pos[,1],pos[,2]) )  
}


# Helper function for str.replace.by.blocks
# an island is a region corresponding to the interior of one block
# an island has i) mountains: sub regions with level above the islands level
#               ii) plains  : the pos.complement to mountains within the island
replace.island = function(island.row, str,blocks, pattern.plains, level,pattern.number.mountains,replacement,sub.txt,fixed=TRUE) {
  restore.point("replace.island")
  
  left = blocks$inner[island.row,1]
  right = blocks$inner[island.row,2]
  island.str = substring(str,left,right)
  
  mountains= blocks$inner[
    which(blocks$levels == level+1
          & blocks$inner[,1]>=left
          & blocks$inner[,2]<=right),,drop=FALSE]
  plains = pos.complement(mountains, start=left, end=right)
  
  
  show.blocks(blocks,str)
  island.row
  
  show.pos(cbind(left,right),str)    
  show.pos(mountains,str)    
  show.pos(plains,str)    
  
  plains.str = str.at.pos(str,plains)
  
  # The island has not enough plains to match the pattern
  if (length(plains.str)<length(pattern.plains))
    return(list(replaced=FALSE,new=island.str,old=island.str))
  
  # Pattern has no mountains, i.e. we simply ignore the mountains in the replacement
  if (length(pattern.plains)==1) {
    ignore.pos = cbind(mountains-left+1)
    new.island.str = str.replace(island.str, pattern.plains,ignore.pos = ignore.pos,fixed=fixed)
    return(list(replaced= new.island.str!=island.str,new.island.str,island.str))
  }
  
  # We have an island with mountains. We search for matching chains of plains
  
  # Search through the different pattern plains

  # Starting plain: must match at end
  i = 1  
  first.pos = str.locate.at.end(plains.str,pattern.plains[i],fixed=fixed)
  matches = !is.na(first.pos[,1])
  
  if (sum(matches)==0)
    return(list(replaced=FALSE,new=island.str,old=island.str))
  
  # Center plains,must match completely
  if (length(pattern.plains)>2) {
    for (i in 2:(length(pattern.plains)-1)) {
      new.matches = str.matches.pattern(plains.str[-(1:(i-1))], pattern.plains[i],fixed=fixed)
      matches = matches & c(new.matches,rep(FALSE,i-1))
    }      
  }
  
  # The last plain must match at the start
  i = length(pattern.plains)    
  # Starting plain: must match at end
  last.pos = str.locate.at.start(plains.str,pattern.plains[i],fixed=fixed)
  matches = matches & c(!is.na(last.pos[,1])[-(1:(i-1))], rep(FALSE,i-1))
  
  if (sum(matches)==0)
    return(list(replaced=FALSE,new=island.str,old=island.str))
  
  # We have found matches to be replaced
  start.with.mountain = plains[1,1]>mountains[1,1]
  mountains.str = str.at.pos(str,mountains)
  nm = pattern.number.mountains
  np =length(pattern.plains)
  
  # The following loop construction rules out overlapping replacements
  counter = 0
  new.str = NULL
  replace.pos = matrix(NA,0,2)
  match.ind = 1
  while (match.ind <= length(matches)-np+1) {
    #message("replace.island(match.ind=",match.ind,")")
    match.ind = which(matches & match.ind <= 1:length(matches) )[1]
    new.str = c(new.str,
                str.replace.list(replacement,
                                 pattern=paste0("_",sub.txt,1:nm,"_"),
                                 replacement=mountains.str[(match.ind:(match.ind+nm))+start.with.mountain])
    )
    
    #plains.str[match.ind+np-1]
    replace.left = plains[match.ind,1] + first.pos[match.ind,1]-left
    replace.right = plains[match.ind+np-1,1] + last.pos[match.ind+np-1,2]-left
    replace.pos = rbind(replace.pos,c(replace.left,replace.right))
    #show.pos(replace.pos,str)
    
    # The last plain may be overlapping
    # This is a bit dirty.... need to think about some better code...
    match.ind = match.ind + max(np-1,1)      
  }
  show.pos(replace.pos, island.str)
  new.island.str = str.replace.at.pos(island.str,replace.pos, new.str)
  return(list(replaced=TRUE,new=new.island.str,old=island.str))
}

#' Helper function
adapt.pos.after.replace = function(pos,left,len.old,len.new) {
  if (length(left)>1) {
    for (i in seq_along(left)) {
      pos = adapt.pos.after.replace(pos,left[i],len.old[i],len.new[i])
    }
    return(pos)
  }
  restore.point("adapt.pos.after.replace")
  right = left + len.old-1
  delta.len = len.new-len.old
  
  rows = pos[,1]>left
  pos[rows,1] = pos[rows,1]+delta.len
  rows = pos[,2]>left
  pos[rows,2] = pos[rows,2]+delta.len
  return(pos)
}

#' Helper function
adapt.blocks.after.replace = function(block,...) {
  block$inner = adapt.pos.after.replace(block$inner,...)
  block$outer = adapt.pos.after.replace(block$outer,...)
  return(block)
}


#' Replaces in str every occurence of pattern by replacement
#' 
#' @param str the string to replaced
#' @param pattern the substring to be replaced
#' @param replacment the new substrings
#' @param block a block retrieved from str.block.pos alternatively, you can provide block.start and block.end
#' @param block.start string with which the blocks start, e.g. "("
#' @param block.end string with which the blocks end, e.g. ")"
#' @param only.replace.smaller.than if not NULL only replaces matches whose number of characters is less or equal to only.replace.smaller.than
#' @param only.replace.larger.than if not NULL only replaces matches whose number of characters is bigger or equal to only.replace.larger.than
#' @return a string
#' @export
str.replace.by.blocks = function(str,pattern,replacement,blocks=NULL,sub.txt="SUB",block.start, block.end,block.ignore=NULL,use.levels=NULL,fixed=TRUE, only.replace.smaller.than=NULL, only.replace.larger.than=NULL) {
  restore.point("str.replace.by.level")
  library(data.table)
  
  if (is.null(blocks))
    blocks = str.blocks.pos(str, start=block.start, end=block.end, ignore=block.ignore, fixed=fixed)
    
  if (length(blocks$levels)==0) {
    blocks = blocks.add.level.0(blocks,str)
  } else if ( blocks$levels[1]!=0) {
    blocks = blocks.add.level.0(blocks,str)    
  }
  
  
  show.blocks(blocks,str)
  levels = blocks$levels
  if (is.null(use.levels))
    use.levels = unique(levels)

  sub.pattern = paste0("_",sub.txt,"_")
  
  # Splitt pattern in different parts before and after ignore
  pattern.plains = str.at.pos(pattern,
          pos.complement(str_locate_all(pattern,sub.pattern)[[1]], str=pattern))
  
  pattern.number.mountains = str.number.matches(pattern,sub.pattern,fixed=TRUE)
  
  level = 0
  old.str = str
  old.blocks = blocks
  for (level in rev(use.levels)) {    
    #message("level = ", level)
    island.rows = which(levels==level)     
    ret =lapply(island.rows,replace.island,str=str,blocks=blocks, pattern.plains=pattern.plains, level=level,pattern.number.mountains=pattern.number.mountains,replacement=replacement,fixed=fixed,sub.txt=sub.txt)
    
    df = data.frame(rbindlist(ret),island.rows)
    df = df[df[,"replaced"],]
    if (!is.null(only.replace.larger.than))
      df = df[nchar(df$old)>=only.replace.larger.than,]
    if (!is.null(only.replace.smaller.than))
      df = df[nchar(df$old)<=only.replace.smaller.than,]
    
    
    str = str.replace.at.pos(str,blocks$inner[df$island.rows,,drop=FALSE],df$new)
    blocks = adapt.blocks.after.replace(blocks,left=blocks$inner[df$island.rows,],len.old=nchar(df$old),len.new=nchar(df$new)) 
    show.blocks(blocks,str)
    
  }
  
  return(str)
}


examples.str.replace.by.blocks = function() {
  # Replace latex fractions
  str = "5+\\frac{x^2+x^2}{1+\\frac{2}{x*5}}*2"
  str.replace.by.blocks(str,"\\frac{_SUB_}{_SUB_}","(_SUB1_)/(_SUB2_)",
                        block.start = "{", block.end = "}")  
  str.replace.by.blocks(str,"\\frac{_SUB_}{_SUB_}","(_SUB1_)/(_SUB2_)",
                        block.start = "{", block.end = "}",
                        only.replace.larger.than=20)  
  str.replace.by.blocks(str,"\\frac{_SUB_}{_SUB_}","(_SUB1_)/(_SUB2_)",
                        block.start = "{", block.end = "}",
                        only.replace.smaller.than=20)  
  str ="-\\frac{\\sigma_{m}-\\beta\\sigma_{b}}{\\beta-1}=\\frac{\\sigma_{m}-\\beta\\sigma_{b}}{1-\\beta}"
  
  str ="\\frac{1}{2}=\\frac{3}{4}"
  str.replace.by.blocks(str,"\\frac{_SUB_}{_SUB_}","(_SUB1_)/(_SUB2_)",
                        block.start = "{", block.end = "}")  
  
}


#' Add level 0 to blocks
blocks.add.level.0 = function(blocks,str,end=nchar(str)) {
  blocks$inner = rbind(c(1,end),blocks$inner)
  blocks$outer = rbind(c(1,end),blocks$outer)
  blocks$levels = c(0,blocks$levels)
  return(blocks)
}

#' Returns a pos matrix indicating blocks like brackets ( ) or quoted parts "text"
#' 
#' We allow for nested blocks. The position matrix also has an attribute level that describes the level of each block
#' 
#' @export
str.blocks.pos= function(str, start, end,
  ignore = NULL, ignore.start = ignore, ignore.end = ignore, 
  fixed = TRUE,fixed.start = fixed, fixed.end = fixed) {

  restore.point("str.blocks.pos")                                                          
  if (length(str) > 1)
    stop("Not yet implemented for vectors of strings")
  
  # Blocks like (),{},[], begin end, ...
  if (start != end) {
    start.pos = str.locate.all(str, start, ignore=ignore.start,fixed=fixed.start)[[1]]
    end.pos   = str.locate.all(str, end, ignore=ignore.end,fixed=fixed.start)[[1]]
    # Validity check
    if (NROW(start.pos) != NROW(end.pos)) {
			print(paste("Error when finding ",start,end, "block in"))
			print(str)
      stop("Number of block starts and ends differs!")
    }
    
    n = NROW(start.pos)
    if (n==0)
      return(list(inner=start.pos, outer=start.pos, levels=c()))
    
    pos.levels = rep(NA,n)
    
    # Compute level
    all = c(start.pos[,2],end.pos[,1])
    ord = order(all)
    ind = c(1:n,1:n)[ord]
    open = c(rep(1,n),rep(-1,n))[ord]
    levels = cumsum(open)
    
    pos.levels[ind[open==1]] = levels[open==1] 

    #Highly inefficient, should write C code here
    end.ord = rep(NA,n)
    used.start = rep(FALSE,n)
    for (i in 1:n) {
      ind = which(start.pos[,2]<end.pos[i,1] & !used.start)
      ind = ind[length(ind)]
      used.start[ind]=TRUE
      end.ord[i]=ind
    }
    end.pos[end.ord,] = end.pos
    return(list(outer=cbind(start.pos[,1],end.pos[,2]),
                inner=cbind(start.pos[,2]+1,end.pos[,1]-1),
                levels=pos.levels))
    
  # Blocks like "" ''
  } else {
    pos = str.locate.all(str, start, ignore=ignore.start, fixed=fixed)[[1]]
    n = NROW(pos)
		
		if (n>0) {
			if ((n %% 2) != 0)
				stop(paste("Number of block starts and ends differs! Need even number of not ignored", start))
			start.pos = pos[seq(1,n,by=2),,drop=FALSE]
			end.pos = pos[seq(2,n,by=2),,drop=FALSE]
      
			return(list(inner=cbind(start.pos[,2]+1,end.pos[,1]-1),
			            outer=cbind(start.pos[,1],end.pos[,2]),
			            levels=rep(1,n/2)))
		} else {
		  return(list(inner=start.pos, outer=start.pos, levels=c()))
		}
  }
}


examples.str.blocks.pos = function() {
  str = '1+(5*(2+3)+(2+(4-1)))'
#        123456789012345678901  
  str.blocks.pos(str,"(",")")
  
}
