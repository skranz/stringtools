example.replace.german.umlaute = function()
{
  setwd("D:/libraries/RTutor/examples")
  file = "Blatt4_sol.rmd"
  txt = replace.german.umlaute(file=file)
}

#' replaces German Umlaute with ascii letters oe, ue, ae
replace.german.umlaute = function(txt=readLines(file), file=NULL, write.file = !is.null(file)) {
  txt = gsub("?","ae",txt, fixed=TRUE)
  txt = gsub("?","oe",txt, fixed=TRUE)
  txt = gsub("?","ue",txt, fixed=TRUE)
  txt = gsub("?","ss",txt, fixed=TRUE)

  txt = gsub("?","Ae",txt, fixed=TRUE)
  txt = gsub("?","Oe",txt, fixed=TRUE)
  txt = gsub("?","Ue",txt, fixed=TRUE)
  
  if (write.file) {
    writeLines(txt,file)
  }
  invisible(txt)
}