source("global.R")

min_date <- as.Date("2010-01-01")
max_date <- as.Date("2022-07-13")

listings <- get_listing_status_av(min_date = min_date, max_date = max_date) %>%
  filter(assetType == "ETF", name %like% "iShares")



etf_daily_adj_data <- get_prices_and_returns_av(
  choosen_tickers = listings$symbol,
  min_date = min_date,
  max_date = max_date
)

save(etf_daily_adj_data, listings, file="data/etf_daily_adj_data.rdata")


