assl <- function(...){
  attribut_list <- list(...)
  for(x in names(attribut_list)[names(attribut_list) != ""]){
    assign(x,attribut_list[[x]], envir = .GlobalEnv)
  }
}



print_fun <- function(fun){
  cap <- capture.output(eval(parse(text=fun)))
  cap[1] <- paste0(fun," <- ",cap[1])
  return(cat(cap, sep="\n"))
}


p0 <- function(...){
  print(paste0(...))
}

m0 <- function(...){
  message(paste0(...))
}


w0 <- function(...){
  warning(paste0(...))
}



buffer <- function(expr, rdata_file, force = FALSE){
  if(!dir.exists("buffer_data")){
    dir.create("buffer_data")
  }
  file <- if(rdata_file %like% ".rdata"){rdata_file}else{paste0(rdata_file,".rdata")}
  file <- paste0("buffer_data/", file)
  if(file.exists(file) && !force){
    load(file)
  }else{
    temp <- eval(expr)
    save(temp, file=file)
  }
  return(temp)
}
