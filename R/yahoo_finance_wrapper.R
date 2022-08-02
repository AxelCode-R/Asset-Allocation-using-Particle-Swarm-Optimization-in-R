
get_prices_and_returns_yf <- function(tickers, from="2020-01-01", to="2021-01-01", type="adjusted", print=F){
  # load("C:/Users/Axel/Desktop/Master-Thesis-All/Master-Thesis/data/spx_composition.rdata")
  # tickers <- unique(spx_composition$Ticker)
  # from <- "2020-01-01"
  # to <- "2021-01-01"

  e <- new.env()

  #getSymbols("%5EGSPC", src="yahoo", env = e)
  try({
    info <- suppressMessages(suppressWarnings(quantmod::getSymbols(tickers, env = e, from = as.Date(from)-days(5), to = to)))
    if(print){print(info)}
  },silent = T)

  prices <- NULL
  for(name in names(e)){
    x = e[[name]]
    if(nrow(x) > 0 && sum(toupper(colnames(x)) %like% toupper(type))==1){
      x = data.frame(x)
      prices <- cbind.xts(prices, setNames(xts(x[,toupper(colnames(x)) %like% toupper(type)], order.by = as.Date(rownames(x))), name))
    }
  }

  if(is.null(prices)){
    return("No Prices found!")
  }else{
    returns <- pri_to_ret(prices)

    data <- list(
      "prices" = prices[paste0(from,"/",to),],
      "returns" = returns[paste0(from,"/",to),]
    )

    return(data)
  }

}






