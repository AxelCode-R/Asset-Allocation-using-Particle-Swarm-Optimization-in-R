
get_listing_status_av <- function(choosen_tickers, max_date, min_date, min_date_history){
  if(!exists("api_key")){
    warning("no api_key found in GlobalEnv.")
    return(NULL)
  }

  data <- suppressMessages({
    httr::content(
      httr::GET(paste0("https://www.alphavantage.co/query?function=LISTING_STATUS&date=",max_date,"&apikey=",api_key,"&datatype=csv&state=active")),
      as = "text",
      encoding = "UTF-8"
    ) %>%
      read_csv() %>%
      filter(ipoDate <= as.Date(min_date)-min_date_history) %>%
      select(-c(delistingDate))
  })


  if(is.null(choosen_tickers)){return(data)}

  data <- data %>%
    filter(symbol %in% choosen_tickers)

  return(data)
}



my_spread <- function(data, date_col, key, value){
  data <- data %>% ungroup()
  nc <- data %>% pull(key) %>% unique() %>% length()
  nr <- data %>% pull(date_col) %>% unique() %>% length()
  mat <- matrix(NA, ncol = nc, nrow=nr)
  colnames(mat) <- data %>% pull(key) %>% unique()
  mat <- xts(mat, order.by = data %>% pull(date_col) %>% unique() %>% as.Date())
  for(i in 1:nc){
    if(i%%10==0){print(i)}
    ticker <- colnames(mat)[i]
    temp <- data %>% filter(!!sym(key)==!!ticker) %>% select(any_of(c(date_col, value)))
    mat[temp %>% pull(date_col),i] <- temp %>% pull(value) %>% as.numeric()
  }
  return(mat)
}



get_prices_and_returns_av <- function(choosen_tickers, max_date, min_date, min_date_history){
  if(!exists("api_key")){
    warning("no api_key found in GlobalEnv.")
    return(NULL)
  }

  wait_sek <- 3

  data <- list(
    daily_prices = list(),
    daily_returns = list()
  )



  print(" ")
  print("daily_data")
  daily_data_raw <- list()
  for(i in 1:length(choosen_tickers)){
    ticker <- choosen_tickers[i]
    if(i%%10==0){
      print(i)
    }

    start_time <- Sys.time()
    suppressMessages(while(as.numeric(Sys.time()-start_time) <= wait_sek){
      try({
        temp_daily_return <- httr::content(
          httr::GET(paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=",ticker,"&outputsize=full&apikey=",api_key,"&datatype=csv")),
          as = "text",
          encoding = "UTF-8"
        )

        if(substr(temp_daily_return, 1, 9) == "timestamp"){
          daily_data_raw[[i]] <- cbind(
            "ticker"=ticker,
            temp_daily_return %>%
              read_csv() %>%
              filter(timestamp>=min_date-min_date_history)
          )
          break
        }else if(substr(temp_daily_return,8,11) == "Note" && !"Error.Message" %in% names(temp_daily_return)){
          cat("+")
          Sys.sleep(1)
        }else{
          print("Not found!")
          break
        }
      })
    })

  }

  daily_data_raw <- bind_rows(daily_data_raw)

  daily_data_raw <- daily_data_raw %>%
    arrange(ticker, timestamp) %>%
    group_by(ticker) %>%
    mutate(
      "return_adj_close" = adjusted_close/dplyr::lag(adjusted_close, order_by = timestamp)-1
    ) %>%
    filter(!is.na(return_adj_close))



  if(nrow(daily_data_raw)>0){
    data$daily_returns <- my_spread(data=daily_data_raw, date_col="timestamp", key="ticker", value="return_adj_close")
    #data$daily_returns <- data$daily_returns[,choosen_tickers]
    data$daily_returns <- na.locf(data$daily_returns)

    data$daily_prices <- my_spread(data=daily_data_raw, date_col="timestamp", key="ticker", value="adjusted_close")
    #data$daily_prices <- data$daily_prices[,choosen_tickers]
    data$daily_prices <- na.locf(data$daily_prices)
  }



  return(data)
}




get_indicator_av <- function(indicator = "RSI", indicator_string = "function=RSI&interval=daily&time_period=10&series_type=open", choosen_tickers, max_date, min_date, min_date_history){
  if(!exists("api_key")){
    warning("no api_key found in GlobalEnv.")
    return(NULL)
  }

  wait_sek <- 3

  data_raw <- list()


  for(i in 1:length(choosen_tickers)){
    ticker <- choosen_tickers[i]
    if(i%%10==0){
      print(i)
    }

    start_time <- Sys.time()
    worked <- FALSE
    suppressMessages(while(as.numeric(Sys.time()-start_time) <= wait_sek && !worked){
      try({
        temp <- httr::content(
          httr::GET(paste0("https://www.alphavantage.co/query?", indicator_string, "&symbol=",ticker,"&apikey=",api_key,"&datatype=csv")),
          as = "text",
          encoding = "UTF-8"
        ) %>%
          read_csv() %>%
          filter(time >= min_date-min_date_history)

        if(nrow(temp)>0){
          data_raw[[i]] <- cbind("ticker"=ticker, temp)
          worked <- TRUE
        }
      })
    })

  }

  data_raw <- bind_rows(data_raw)

  if(nrow(data_raw)>0){
    data <- my_spread(data=data_raw, date_col="time", key="ticker", value=indicator)
  }


  return(data)
}



get_endpoints <- function(keywords){
  if(!exists("api_key")){
    warning("no api_key found in GlobalEnv.")
    return(NULL)
  }

  wait_sek <- 3

  data_raw <- list()


  for(i in 1:length(keywords)){
    key <- keywords[i]
    if(i%%10==0){
      print(i)
    }

    start_time <- Sys.time()
    worked <- FALSE
    suppressMessages(while(as.numeric(Sys.time()-start_time) <= wait_sek && !worked){
      try({
        temp <- httr::content(
          httr::GET(paste0("https://www.alphavantage.co/query?function=SYMBOL_SEARCH&keywords=",key,"&apikey=",api_key,"&datatype=csv")),
          as = "text",
          encoding = "UTF-8"
        ) %>%
          read_csv()

        if(nrow(temp)>0 && ncol(temp)>1){
          data_raw[[i]] <- cbind("listings_name"=key, temp)
          worked <- TRUE
        }
      })
    })

  }

  data_raw <- bind_rows(data_raw)


  return(data_raw)
}
