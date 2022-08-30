#' calculate continius cummulated returns from returns
#' @export
ret_to_cumret <- function(data_xts){
  cumprod(1+rbind.xts(
    xts(matrix(rep(0,ncol(data_xts)), ncol=ncol(data_xts), dimnames = list(NULL, colnames(data_xts))), order.by = min(index(data_xts))-1),
    data_xts
  ))*100
}


pri_to_ret <- function(data_xts){
  data_xts <-  data_xts/lag.xts(data_xts) - 1

  return(data_xts[-1,])
}

mrunif <- function(nr, nc, lower, upper) {
  return(matrix(runif(nr*nc,0,1),nrow=nr,ncol=nc)*(upper-lower)+lower)
}
