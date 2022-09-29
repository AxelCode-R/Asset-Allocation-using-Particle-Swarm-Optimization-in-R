#' calculate continius cummulated returns from returns
#' @export
ret_to_cumret <- function(data_xts){
  cumprod(1+rbind.xts(
    xts(matrix(rep(0,ncol(data_xts)), ncol=ncol(data_xts), dimnames = list(NULL, colnames(data_xts))), order.by = min(index(data_xts))-1),
    data_xts
  ))*100
}


pri_to_ret <- function(data_xts){
  data_xts <- na.locf(data_xts)
  data_xts <-  data_xts/lag.xts(data_xts) - 1
  return(data_xts[-1,])
}

mrunif <- function(nr, nc, lower, upper) {
  return(matrix(runif(nr*nc,0,1),nrow=nr,ncol=nc)*(upper-lower)+lower)
}

ret_to_geomeanret <- function(xts_ret){
  sapply((1+xts_ret), prod)^(1/nrow(xts_ret))-1
}


calc_portfolio_returns <- function(xts_returns, weights, name="portfolio"){
  if(sum(weights)!=1){
    xts_returns$temp___X1 <- 0
    weights <- c(weights, 1-sum(weights))
  }
  res <- cumprod((1+xts_returns)) * matrix(
    rep(weights, nrow(xts_returns)), ncol=length(weights), byrow=T)
  res <- xts(
    rowSums(res/c(1, rowSums(res[-nrow(xts_returns),])))-1,
    order.by=index(xts_returns)) %>%
    setNames(., name)
  return(res)
}



cov_ <- function(mat, mean_vec=NULL){
  if(is.null(mean_vec)){
    mat_mean <- matrix(data=1, nrow=nrow(mat)) %*% apply(mat, 2, mean)
  }else{
    mat_mean <- matrix(data=1, nrow=nrow(mat)) %*% mean_vec
  }
  mat <- mat - mat_mean

  return((nrow(mat)-1)^(-1) * t(mat) %*% mat)
}
