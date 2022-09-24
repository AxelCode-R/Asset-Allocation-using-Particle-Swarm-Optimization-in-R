
get_yf <- function(tickers, from="2018-01-01", to="2019-12-31", price_type="close", return_type="adjusted", print=F){

  e <- new.env()

  try({
    info <- suppressMessages(suppressWarnings(quantmod::getSymbols(tickers, env = e, from = as.Date(from)-days(5), to = to)))
    if(print){print(info)}
  },silent = T)

  prices <- NULL
  prices_for_returns <- NULL
  for(name in names(e)){
    x = e[[name]]
    if(nrow(x) > 0 && sum(toupper(colnames(x)) %like% toupper(price_type))==1){
      x = data.frame(x)
      prices <- cbind.xts(prices, setNames(xts(x[,toupper(colnames(x)) %like% toupper(price_type)], order.by = as.Date(rownames(x))), name))
      prices_for_returns <- cbind.xts(prices_for_returns, setNames(xts(x[,toupper(colnames(x)) %like% toupper(return_type)], order.by = as.Date(rownames(x))), name))
    }
  }

  data <- list()

  if(!is.null(prices_for_returns)){
    data$returns <- pri_to_ret(prices_for_returns)[paste0(from,"/",to),]
  }

  if(!is.null(prices)){
    data$prices <- prices[paste0(from,"/",to),]
  }

  return(data)

}






