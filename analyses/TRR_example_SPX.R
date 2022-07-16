source("global.R")
# file:///C:/Users/Axel/Desktop/Master-Thesis-All/Ziel%20was%20beantwortet%20werden%20soll/Quellen%20nur%20wichtige/gut_2_Yuen2021_Article_AMetaheuristic-basedFrameworkF.pdf



# DATA
load("C:/Users/Axel/Desktop/Master-Thesis-All/Master-Thesis/data/spx_index_daily_adj_data.rdata")
load("C:/Users/Axel/Desktop/Master-Thesis-All/Master-Thesis/data/spx_daily_adj_data.rdata")
load("C:/Users/Axel/Desktop/Master-Thesis-All/Master-Thesis/data/spx_composition.rdata")



# formeln: tr2_16.pdf
train_months <- 36
test_months <- 3

date <- as.Date("2017-02-01")
train_period <- seq.Date(date-months(train_months), date-days(1), by="day")
train_period_xts <- paste0(min(train_period),"/",max(train_period))
test_period <- seq.Date(date, date + months(test_months), by="day")
test_period_xts <- paste0(min(test_period),"/",max(test_period))
valid_tickers <- spx_composition %>%
  filter(Date <= date) %>% filter(Date == max(Date)) %>% pull(Ticker) %>% .[. %in% colnames(spx_daily_adj_data$daily_returns)]
asset_returns_test <- spx_daily_adj_data$daily_returns[train_period_xts, valid_tickers]
asset_returns_test <- asset_returns_test[,colSums(is.na(asset_returns_test))==0]
bm_returns_test <- spx_index_daily_adj_data$daily_returns[train_period_xts, ]
#bm_returns_test <- xts(asset_returns_test %*% rep(1/ncol(asset_returns_test), ncol(asset_returns_test)), order.by = index(asset_returns_test))


mat <- list(
    Dmat = cov(asset_returns_test, asset_returns_test),
    dvec = as.numeric(var(bm_returns_test)) * t(cov(asset_returns_test, bm_returns_test)/as.numeric(var(bm_returns_test))),
    Amat = t(rbind(
      rep(1, ncol(asset_returns_test)))
    ),
    bvec = 1,
    meq = 1
)

if(!is.positive.definite(mat$Dmat)){
  print("make Dmat positiv definit!")
  temp <- as.matrix(nearPD(mat$Dmat)$mat)
  print(paste0("max change: ", (mat$Dmat-temp) %>% max(), "   mean change: ", (mat$Dmat-temp) %>% mean()))
  mat$Dmat <- temp
}

qp <- solve.QP(Dmat = mat$Dmat, dvec = mat$dvec, Amat = mat$Amat, bvec = mat$bvec, meq = mat$meq)

port_returns_test <- setNames(asset_returns_test %*% qp$solution, "port_returns_test")
qp_goodness <- as.numeric(var(port_returns_test - bm_returns_test))


plotly_line_chart_xts(ret_to_cumret(cbind.xts(port_returns_test, bm_returns_test)))




