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
