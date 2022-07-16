source("global.R")

min_date <- as.Date("2005-01-01")
max_date <- as.Date("2022-07-13")


# GET COMPOSITION OF STX
wikispx <- read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies')
currentconstituents <- wikispx %>%
  html_node("#constituents") %>%
  html_table(header = TRUE)

spxchanges <- wikispx %>%
  html_node("#changes") %>%
  html_table(header = FALSE, fill = TRUE) %>%
  filter(row_number() > 2) %>% # First two rows are headers
  `colnames<-`(c('Date','AddTicker','AddName','RemovedTicker','RemovedName','Reason')) %>%
  mutate(Date = as.Date(Date, format = "%B %d, %Y"),
         year = year(Date),
         month = month(Date))


# Start at the current constituents…
currentmonth <- as.Date(format(Sys.Date(), '%Y-%m-01'))
monthseq <- seq.Date(as.Date('1990-01-01'), currentmonth, by = 'month') %>% rev()

spxstocks <- currentconstituents %>% mutate(Date = currentmonth) %>% select(Date, Ticker = Symbol, Name = Security)
lastrunstocks <- spxstocks

# Iterate through months, working backwards
for (i in 2:length(monthseq)) {
  d <- monthseq[i]
  y <- year(d)
  m <- month(d)
  changes <- spxchanges %>%
    filter(year == year(d), month == month(d))

  # Remove added tickers (we’re working backwards in time, remember)
  tickerstokeep <- lastrunstocks %>%
    anti_join(changes, by = c("Ticker" = "AddTicker")) %>%
    mutate(Date = d)

  # Add back the removed tickers…
  tickerstoadd <- changes %>%
    filter(!RemovedTicker == "") %>%
    transmute(Date = d,
              Ticker = RemovedTicker,
              Name = RemovedName)

  thismonth <- tickerstokeep %>% bind_rows(tickerstoadd)
  spxstocks <- spxstocks %>% bind_rows(thismonth)

  lastrunstocks <- thismonth
}
spx_composition <- spxstocks %>% filter(Date >= as.Date(min_date))

save(spx_composition, file="data/spx_composition.rdata")



# GET DAILY RETURNS AND PRICES FOR ALL SPX TICKERS
spx_daily_adj_data <- get_prices_and_returns_av(
    choosen_tickers = spx_composition$Ticker %>% unique(),
    min_date = min_date,
    max_date = max_date,
    min_date_history = days(0)
  )

save(spx_daily_adj_data, file="data/spx_daily_adj_data.rdata")



# GET REFERENCE INDEX iShares Core S&P 500 ETF
spx_index_daily_adj_data <- get_prices_and_returns_av(
  choosen_tickers = "IVV",
  min_date = min_date,
  max_date = max_date,
  min_date_history = days(0)
)

save(spx_index_daily_adj_data, file="data/spx_index_daily_adj_data.rdata")




















