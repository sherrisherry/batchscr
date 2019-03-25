# codes passed as args are all executed in parent environment in the order of their position in the function body;
# code and ehandle works in the parent environment;
# thus ehandle, max_try, cond can refer to vars inside code;
# due to position in function body, no arg can refer to vars in ehandle.
# cond is an additional condition for success of 'code'
ecycle <- function(code, ehandle, max_try, thandle, ecorrect, cond = TRUE){
  trycount <- 1
  repeat{
    msg <- try(code)
    trycount <- trycount + 1
    if(class(msg)!='try-error' && cond || trycount > max_try)break
	if(!missing(ecorrect))ecorrect
  }
  if(class(msg)=='try-error' || !cond)ehandle
  else if(missing(thandle))invisible(msg)
		else thandle
}

mklog <- function(x, path, sep = '\t'){
  txt <- paste(Sys.time(), Sys.timezone(), x, sep = sep)
  cat(paste(txt,'\n', sep=''), file = path, append = TRUE)
}
