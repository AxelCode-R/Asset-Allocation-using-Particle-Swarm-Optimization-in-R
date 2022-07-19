source("global.R")
# file:///C:/Users/Axel/Desktop/Master-Thesis-All/Ziel%20was%20beantwortet%20werden%20soll/Quellen%20nur%20wichtige/gut_2_Yuen2021_Article_AMetaheuristic-basedFrameworkF.pdf



# DATA
load("C:/Users/Axel/Desktop/Master-Thesis-All/Master-Thesis/data/spx_index_daily_adj_data.rdata")
load("C:/Users/Axel/Desktop/Master-Thesis-All/Master-Thesis/data/spx_daily_adj_data.rdata")
load("C:/Users/Axel/Desktop/Master-Thesis-All/Master-Thesis/data/spx_composition.rdata")



# formeln: tr2_16.pdf
train_months <- 6
test_months <- 3
date <- as.Date("2017-02-01")


train_period_xts <- paste0(date-months(train_months), "/", date-days(1))
test_period_xts <- paste0(date, "/", date + months(test_months))
valid_tickers <- spx_composition %>%
  filter(Date <= date) %>% filter(Date == max(Date)) %>% pull(Ticker) %>% .[. %in% colnames(spx_daily_adj_data$daily_returns)]
asset_returns_train <- spx_daily_adj_data$daily_returns[train_period_xts, valid_tickers] %>% .[,colSums(is.na(.))==0]
asset_returns_test <- spx_daily_adj_data$daily_returns[test_period_xts, colnames(asset_returns_train)] %>% .[,colSums(is.na(.))==0]
if(ncol(asset_returns_train) != ncol(asset_returns_train)){ message("Assets in train and test are diffrent!") }
bm_returns_train <- spx_index_daily_adj_data$daily_returns[train_period_xts, ]
bm_returns_test <- spx_index_daily_adj_data$daily_returns[test_period_xts, ]
#bm_returns_train <- xts(asset_returns_train %*% rep(1/ncol(asset_returns_train), ncol(asset_returns_train)), order.by = index(asset_returns_train))

asset_prices_train <- spx_daily_adj_data$daily_prices[train_period_xts, valid_tickers] %>% .[,colSums(is.na(.))==0]
bm_prices_train <- spx_index_daily_adj_data$daily_prices[train_period_xts, ]
asset_mean_ret <- (last(asset_prices_train)/first(coredata(asset_prices_train)))^(1/(nrow(asset_prices_train)-1))-1
bm_mean_ret <- as.numeric((last(bm_prices_train)/first(coredata(bm_prices_train))))^(1/(nrow(bm_prices_train)-1))-1
beta_vec <- cov(asset_returns_train, bm_returns_train)/as.numeric(var(bm_returns_train))

lambda <- 0
mat <- list(
    Dmat = cov(asset_returns_train, asset_returns_train),
    dvec = as.vector(as.numeric(var(bm_returns_train)) * t(beta_vec) +
      lambda * (asset_mean_ret - bm_mean_ret)^2),
    Amat = t(rbind(
      rep(1, ncol(asset_returns_train)), # sum up to 1
      diag(1, nrow=ncol(asset_returns_train), ncol=ncol(asset_returns_train)) # long only
    )),
    bvec = c(
      1, # sum up to 1
      rep(0, ncol(asset_returns_train))
    ),
    meq = 1
)

if(!is.positive.definite(mat$Dmat)){
  print("make Dmat positiv definit!")
  temp <- as.matrix(nearPD(mat$Dmat)$mat)
  print(paste0("max change signf: ",  nchar(sub(".*\\.(0*).*","\\1",max(mat$Dmat-temp))), "   mean change signf: ",  nchar(sub(".*\\.(0*).*","\\1",mean(mat$Dmat-temp)))))
  mat$Dmat <- temp
}


# solve.QP
qp <- solve.QP(Dmat = mat$Dmat, dvec = mat$dvec, Amat = mat$Amat, bvec = mat$bvec, meq = mat$meq)

qp_port_returns_train <- setNames(asset_returns_train %*% qp$solution, "qp_port_returns_train")
qp_port_returns_test <- setNames(asset_returns_test %*% qp$solution, "qp_port_returns_test")




# PSO
pso <- psoptim(
  par = rep(1/ncol(mat$Dmat), ncol(mat$Dmat)),
  fn = function(x, mat){
    fit <- 0.5 * t(x) %*% mat$Dmat %*% x - t(mat$dvec) %*% x
    constraint <- - sum(pmin(0, t(mat$Amat) %*% x - mat$bvec))
    return(fit + constraint)
  },
  mat = mat,
  lower = 0,
  upper = 1,
  control = list(
    trace = 1,
    maxit = 50
  )
)


pso_port_returns_train <- setNames(asset_returns_train %*% pso$par, "pso_port_returns_train")
pso_port_returns_test <- setNames(asset_returns_test %*% pso$par, "pso_port_returns_test")








plotly_line_chart_xts(ret_to_cumret(cbind.xts(qp_port_returns_train, pso_port_returns_train, bm_returns_train)))
plotly_line_chart_xts(ret_to_cumret(cbind.xts(qp_port_returns_test, pso_port_returns_test, bm_returns_test)))












