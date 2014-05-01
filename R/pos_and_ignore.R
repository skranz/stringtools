# Some transformations for pos matrices and ignore vectors


pos.vec.as.matrix = function(pos) {
  if (!is.matrix(pos)) {
    pos = cbind(pos,pos)
  }
  return(pos)
}  


#' Returns a pos matrix combined with its complement. The matrix has an attribute "complement" which is a logical vector indicating whether a row in the matrix is the original matrix or a complement
#' @export
pos.with.complement = function(pos, is.sorted=FALSE, end=NULL) {
  restore.point("pos.with.complement")
  pos.complement(pos,is.sorted,keep.pos=TRUE,end=end)
}

#' Returns the complement of a pos matrix again as a pos matrix
#' @export
pos.complement = function(pos, is.sorted=FALSE,start=1,end=NULL, keep.pos = FALSE,str=NULL) {
  restore.point("pos.complement")
  if (is.null(end))
    if (!is.null(str)) {
      end = nchar(str)
    } else {
      end = 1000000L
    }
  
  pos = pos.vec.as.matrix(pos)
  if (!is.sorted) {
    pos = pos[order(pos[,1]),,drop=FALSE]
  }
  
  # Convert to C code to speed up
  
  if (NROW(pos)==0) {
    mat = matrix(c(start,end),1,2)
    attr(mat,"complement")<-TRUE
    return(mat)
  }
  compl = matrix(0,nrow(pos)+1,2)
  
  row.compl = 0 # 0
  last.left = start
  for (i in 1:NROW(pos)) {
    if (pos[i,1]>last.left) {
      row.compl = row.compl+1
      compl[row.compl,] = c(last.left,pos[i,1]-1)
    }
    last.left = max(last.left,pos[i,2]+1)
  }
  row.compl = row.compl+1
  compl[row.compl,] = c(last.left,end)
  compl = compl[1:row.compl,,drop=FALSE]
  if (!keep.pos) {
    return(compl) 
  } else {
    mat = rbind(pos,compl)
    ord = order(mat[,1])
    complement = c(rep(FALSE,NROW(pos)),rep(TRUE,NROW(compl)))[ord]
    mat = mat[ord,]
    attr(mat,"complement")<-complement
    return(mat)
  }
}

examples.pos.complement = function() {
  pos = matrix(c(
    2,2,
    5,8,
    10,10
  ),3,2,byrow=TRUE)
  pos.complement(pos)
  pos.complement(pos,keep.pos=TRUE)  
}


examples.pos.with.complement = function() {
  pos = matrix(c(
    2,2,
    5,8,
    10,10
  ),3,2,byrow=TRUE)
  pos.with.complement(pos)
}

#' Gets a logical ignore vector or list from pos matrices
#' @export
get.ignore = function(ignore=NULL,ignore.pos=NULL,only.pos=NULL, end=NULL, str=NULL) {
  restore.point("get.ignore")
  if (!is.null(ignore)) {
    return(ignore)
  }
  if (!is.null(ignore.pos)) {
    return(pos.to.ignore(ignore.pos,end=end,str=str))
  }
  if (!is.null(only.pos)) {
    return(pos.to.ignore(only.pos,end=end,str=str,complement=TRUE))
  }
  return(NULL)
}

#' Transforms a boolean vector ignore to a pos matrix and its complement
#' 
#' @return A list first element is the pos and complement matrix. The second element is a vector
#' @export
ignore.and.complement.pos = function(ignore=NULL,ignore.pos=NULL,only.pos=NULL) {
  if (is.list(ignore) | is.list(ignore.pos) | is.list(only.pos) ) {
    stop("Vectorization not yet implemented")
    return(lapply(ignore,ignore.and.complement.pos))
  }
  restore.point("ignore.and.complement.pos")
  ig.pos = NULL
  if (!is.null(ignore)) {
    pos = ignore.to.pos(ignore)
    ig.pos = pos.with.complement(pos,end=length(ignore))
    attr(ig.pos,"is.ignored") = !attr(ig.pos,"complement")
  }
  if (!is.null(ignore.pos)) {
    ig.pos = pos.with.complement(ignore.pos)
    attr(ig.pos,"is.ignore") = !attr(ig.pos,"complement")
  }
  if (!is.null(only.pos)) {
    ig.pos = pos.with.complement(only.pos)
    attr(ig.pos,"is.ignore") = attr(ig.pos,"complement")
  }
  ig.pos  
}

#' Transforms a boolean vector ignore to a pos matrix
#' @export
ignore.to.pos = function(ignore,complement=FALSE) {
  if (is.list(ignore)) {
    return(lapply(ignore,ignore.to.pos,complement=complement))
  }
  
  if (complement)
    ignore = !ignore
  
  if (length(ignore)==0)
    return(matrix(NA,0,2))
  
  d = diff(ignore)
  start = which(d==1)+1
  end = which(d==-1)
  if (ignore[1])
    start=c(1,start)
  if (ignore[length(ignore)])
    end = c(end,length(ignore))
  if (length(start)==0)
    return(matrix(NA,0,2))
  return(cbind(start,end))
  
}

examples.ignore.to.pos = function() {
  ignore = c(TRUE,FALSE,TRUE,TRUE,FALSE,TRUE,FALSE)
  ignore.to.pos(ignore)
  
  ignore = list(c(TRUE,FALSE,TRUE,TRUE,FALSE,TRUE,FALSE),c(TRUE),c(),c(FALSE,FALSE))
  ignore.to.pos(ignore)
  ignore = c(TRUE,FALSE,TRUE,TRUE,FALSE,TRUE,FALSE)
  ignore.and.complement.pos(ignore)
  
}


#' Transforms a pos matrix into a logical ignore vector
#' Warning length is not
#' @export 
pos.to.ignore = function(pos, end=1000000L,complement=FALSE, str=NULL) {
  restore.point("pos.to.ignore")
  
  if (is.list(pos)) {
    return(lapply(1:i,function(i) {
      pos.to.ignore(pos[[i]],end=end[min(i,length(end))],complement=complement,str=str[min(i,length(str))])
    }))
  }
  restore.point("pos.to.ignore.single")
  
  if (!is.null(str))
      end = max(nchar(str))
  ignore = rep(FALSE,end)
  if (NROW(pos)==0)
    return(ignore)
  for (i in 1:NROW(pos)) {
    ignore[pos[i,1]:pos[i,2]] = TRUE
  }
  return(ignore)
}

#' Cummulative sum of number of characters that are ignored
#' @export
cumsum.ignore = function(ignore) {
  if (is.matrix(ignore)) {
    return(apply(ignore,1,cumsum))
  }else {
    return(cumsum(ignore))
  }
}
examples.cumsum.ignore = function() {
  str =c("Now that is a nice matrix. Not?","but short!")
  ignore = rep(FALSE,max(nchar(str)))
  ignore[c(4:5,8:20)] = TRUE
  ignore
  cumsum.ignore(ignore)
  ignore = matrix(ignore,nrow=NROW(str),ncol=length(ignore),byrow=TRUE)
  cumsum.ignore(ignore)
}


#' Combine a list of positions via logical AND
#'
#' @export
combine.pos.list.and = function(pos.list,return.ind=TRUE) {
  restore.point("combine.pos.list.and")
  
  stopifnot(return.ind)
  
  n = length(pos.list)
  if (n==2) {
    return(combine.pos.and(pos.list[[1]],pos.list[[2]],return.ind))
  }
  pos.left = pos.list[[1]]
  ind = list()
  pos.ret = list()
  i = 1
  while(i<n) {
    i = i+1
    pos.right = pos.list[[i]]
    ret = combine.pos.and(pos.left,pos.right,return.ind=TRUE)
    ind[[i]] = ret$ind
    pos.left = ret$pos
    if (length(pos.left)==0) {
      return(list(pos= matrix(NA,nrow=0,ncol=n),ind= matrix(NA,nrow=0,ncol=n)))
    }
  }
  ind.mat = matrix(NA,NROW(ret$pos),n)
  
  ind.left = vector("list",n)
  ind.mat[,i]   = ind[[i]][,2]
  ind.left[[i]] = ind[[i]][,1]
  # Now we define ind by going from right to left  
  for (i in (n-1):2) {
    ind.mat[,i]   = ind[[i]][ind.left[[i+1]],2]
    ind.left[[i]] = ind[[i]][ind.left[[i+1]],1]
  }
  ind.mat[,1] = ind.left[[2]]
  return(list(pos=ret$pos,ind=ind.mat))
}
# pos1 = rbind(c(100,104),c(1,1),c(11,12))
# pos2 = rbind(c(103,105),c(2,4),c(13,15))
# pos3 = rbind(c(106,107),c(5,9),c(17,18))
# combine.pos.list.and(list(pos1,pos2,pos3))

#' Combine two positions via logical AND
#' 
#' @export
combine.pos.and = function(pos1,pos2,return.ind=FALSE) {
  restore.point("combine.pos.and")
  #rerestore.point("combine.pos.and")
  
  if (is.empty(pos1) | is.empty(pos2)) { 
    return(EMPTY.POS)
  }
  
  # pos2 is exactly right of pos1 
  am = make.grid.matrix(x=list(1:NROW(pos1),1:NROW(pos2)))
  pos1=pos1[am[,1],,drop=FALSE];pos2=pos2[am[,2],,drop=FALSE]
  
  rows = which(pos2[,1]==pos1[,2]+1)
  pos = cbind(pos1[rows,1],pos2[rows,2])
  if (!return.ind)
    return(pos)
  
  return(list(pos=pos,ind=cbind(am[rows,1],am[rows,2])))
}  		
