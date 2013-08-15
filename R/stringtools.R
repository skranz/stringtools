# #' A package with functions for string and text modifications to complement stringr
# #' 
# #' @name stringtools-package
# #' @docType package
# #' @author Sebastian Kranz \email{sebkranz@gmail.com}


#' pos can be a 
#'   vector: assuming an element of size 1 that specifies a single char at that positions
#'   n*2 matrix: first column left position, right column right position
#'   list of vectors or matrices, specifying different pos for different str

# Special charcters that can appear in find patterns
library(stringr)
library(restorepoint)

DO_CHECK_STR_PAR = TRUE

#' Check if parameter to a str function have allowed dimensions
check.str.par = function(str,para) {  
  if(!DO_CHECK_STR_PAR)
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

#' Finds start and end positions of first substring that matches pattern
#' @param ignore.pos a logical vector or logical matrix indicating which locations of str shall be ignored in the search
#' @return single.return is a 1*2 matrix. First column start position, second column end position
#' @export
str.locate.first = function(str, pattern, fixed=TRUE, perl=FALSE, ignore =NULL, ignore.pos=NULL,pos.only=NULL) {
  restore.point("str.locate.first")

  #print(ignore.pos)
  ignore = get.ignore(ignore,ignore.pos,pos.only,str=str)
  
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
str.locate.all = function(str, pattern, fixed=TRUE, perl=FALSE, ignore =NULL) {
  restore.point("str.locate.all")
  
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
  if (length(str)==1 & !is.matrix(ignore))
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


#' Splits string vectors
#' @param str a vector of strings
#' @param pattern vector where splits take place
#' @return A list with same length as str. Each list element i contains the split substrings from str[i]
#' @export
str.split = function(str,pattern, fixed=TRUE,perl=FALSE, keep.match = FALSE,ncol=c("variable","like.first")[1], ignore=NULL) {
	restore.point("str.split")
	#rerestore.point("str.split")
  
	check.str.par(str,list(pattern=pattern))
	
	stopifnot(length(str)==length(pattern) | length(str)==1 | length(pattern)==1)
	if (length(str)==1)
	  str = rep(str,length.out=length(pattern))
	
  if (ncol=="variable") {
    
    pos = str.locate.all(str=str,pattern=pattern,fixed=fixed,perl=perl, ignore=ignore)
    #str.match.all(str=str,pattern=pattern,fixed=fixed,perl=perl, ignore=ignore)
    return(str.split.at.pos(str,pos,keep.pos=keep.match))
  } else {
	  stop("Fixed col length, not yet implemented!")
  }
}
                    
examples.str.split = function() {
  str <- c("aes_afe_f", "qwe.rty", "yui0op[3", "b")
  #split x on the letter e
  str
  
  ignore = c(FALSE,TRUE,TRUE)
  str.split(str, "e", keep.match=TRUE, ignore=ignore)
  str.split(str, "e", keep.match=TRUE)
  
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
  pos = rbind(c(7,10),c(4,5))
  new = c("XXX","...")
  str.replace.at.pos(str,pos,new)
  
  str = c("1234567890","ahgdasdajsdgadhsabd")
  str.replace.at.pos(str,pos,new)          
}


examples.has.substr = function() {
  str = c("12347382709")
  pattern = c("a","4","56","34","766","b")
  str.has.substr(str,pattern)  
}


#' Replaces in str every occurence of pattern by replacement
#' 
#' @param str the string to replaced
#' @param pattern the substring to be replaced
#' @param replacment the new substrings
#' @return a string
#' @export
str.replace = function(str,pattern,replacement,fixed=TRUE,perl=FALSE,ignore=NULL, ignore.pos=NULL, pos.only=NULL,ignore.pattern="_IGNORE_",...) {
  restore.point("str.replace")
  len = max(length(str),length(pattern),length(replacement)) 
  if (len > 1) {
    ret = sapply(1:len, function (i,...) {
      str.replace(str[min(length(str),i)],
                  pattern[min(length(pattern),i)],
                  replacement[min(length(replacement),i)],
                  fixed, perl,ignore,ignore.pos,pos.only,...)
    },...)
    return(ret)
  }  
  restore.point("str.replace.single")
  
  pos = ignore.and.complement.pos(ignore,ignore.pos,pos.only)  
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
  str.replace(str,pattern,replacement, pos.only=pos)
  str.replace(str,pattern,replacement)
  
  str = "int{5*2}*{2*3}"
  pattern = "int{_IGNORE_}"
  replacement = "integer{_IGNORE_}"  
  pos = cbind(c(5,11),c(7,13))
  str.replace(str,pattern,replacement, ignore.pos=pos)
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
  
  if (fixed.start) start = fixed(start)
  if (fixed.end) end = fixed(end)
  
  # Blocks like (),{},[], begin end, ...
  if (start != end) {
    start.pos = str.locate.all(str, start, ignore=ignore.start)[[1]]
    end.pos   = str.locate.all(str, end, ignore=ignore.end)[[1]]
    # Validity check
    if (NROW(start.pos) != NROW(end.pos)) {
			print(paste("Error when finding ",start,end, "block in"))
			print(str)
      stop("Number of block starts and ends differs!")
    }
    
    n = NROW(start.pos)
    pos.level = rep(NA,n)
    
    # Compute level
    all = c(start.pos[,2],end.pos[,1])
    ord = order(all)
    ind = c(1:n,1:n)[ord]
    open = c(rep(1,n),rep(-1,n))[ord]
    level = cumsum(open)
    
    pos.level[ind[open==1]] = level[open==1] 

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
    return(list(inner=cbind(start.pos[,2],end.pos[,1]),
                outer=cbind(start.pos[,1],end.pos[,2]),
                level=pos.level))
    
  # Blocks like "" ''
  } else {
    pos = str.locate.all(str, start, ignore=ignore.start)[[1]]
    n = length(pos)
		
		if (n>0) {
			if ((n %% 2) != 0)
				stop(paste("Number of block starts and ends differs! Need even number of not ignored", start))
			start.pos = pos[seq(1,n,by=2),]
			end.pos = pos[seq(2,n,by=2),]
      
			return(list(inner=cbind(start.pos[,2]+1,end.pos[,1]-1),
			            outer=cbind(start.pos[,1],end.pos[,2]),
			            level=rep(1,n)))
		} else {
		  return(list(inner=start.pos, outer=start.pos, level=c()))
		}
  }
}


examples.str.blocks.pos = function() {
  str = '1+(5*(2+3)+(2+(4-1)))'
#        123456789012345678901  
  str.blocks.pos(str,"(",")")
}
