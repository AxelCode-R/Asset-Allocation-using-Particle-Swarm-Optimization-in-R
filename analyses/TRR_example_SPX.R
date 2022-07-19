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


mat <- list(
    Dmat = cov(asset_returns_train, asset_returns_train),
    dvec = as.vector(as.numeric(var(bm_returns_train)) * t(beta_vec) + # file:///C:/Users/Axel/Desktop/Master-Thesis-All/Ziel%20was%20beantwortet%20werden%20soll/Quellen%20nur%20wichtige/tr2_16.pdf
      0 * t(bm_returns_train) %*% asset_returns_train + # MSE file:///C:/Users/Axel/Downloads/jcssp.2014.2450.2463.pdf
      0 * (asset_mean_ret - bm_mean_ret)^2),
    Amat = t(rbind(
      rep(1, ncol(asset_returns_train)), # sum up to 1
      diag(1, nrow=ncol(asset_returns_train), ncol=ncol(asset_returns_train)) # long only
    )),
    bvec = c(
      1, # sum up to 1
      rep(0, ncol(asset_returns_train)) # long only
    ),
    meq = 1
)

if(!is.positive.definite(mat$Dmat)){
  print("make Dmat positiv definit!")
  temp <- as.matrix(nearPD(mat$Dmat)$mat)
  print(paste0("max change signf: ",  nchar(sub(".*\\.(0*).*","\\1",max(mat$Dmat-temp))), "   mean change signf: ",  nchar(sub(".*\\.(0*).*","\\1",mean(mat$Dmat-temp)))))
  mat$Dmat <- temp
}

##########################################
# Helper Functions
##########################################

objectiv_fitness <- function(x, mat){
  as.numeric(0.5 * t(x) %*% mat$Dmat %*% x - t(mat$dvec) %*% x)
}

constraint_check <- function(x, mat){
  as.numeric(- sum(pmin(0, t(mat$Amat) %*% x - mat$bvec)))
}

MSE <- function(x){
  as.numeric(sqrt(sum(( x )^2)))
}

##########################################
# Information Datastructure
##########################################

info <- NULL

##########################################
# solve.QP
##########################################

time_it <- system.time({
  qp <- solve.QP(Dmat = mat$Dmat, dvec = mat$dvec, Amat = mat$Amat, bvec = mat$bvec, meq = mat$meq)
})


qp_port_returns_train <- xts(asset_returns_train %*% qp$solution, order.by = index(asset_returns_train)) %>% `colnames<-`(., "qp_port_returns_train")
qp_port_returns_test <- xts(asset_returns_test %*% qp$solution, order.by = index(asset_returns_test)) %>% `colnames<-`(., "qp_port_returns_test")

qp_alpha_train <- ret_to_cumret(qp_port_returns_train) - ret_to_cumret(bm_returns_train)
qp_alpha_test <- ret_to_cumret(qp_port_returns_test) - ret_to_cumret(bm_returns_test)

info <- bind_rows(
  info,
  data.frame(
    "date" = date,
    "optimizer" = "QP",
    "objectiv_fitness" = objectiv_fitness(qp$solution, mat),
    "constraint_check" = constraint_check(qp$solution, mat),
    "real_MSE_train" = MSE(qp_alpha_train),
    "real_MSE_test" = MSE(qp_alpha_test),
    "raw_MSE_train" = MSE(qp_port_returns_train - bm_returns_train),
    "raw_MSE_test" = MSE(qp_port_returns_test - bm_returns_test),
    "time" = time_it[3]
  )
)

##########################################
# PSO
##########################################

pso_fn = function(x){
  fit <- objectiv_fitness(x, mat)
  constraint <- constraint_check(x, mat)

  return(fit + constraint)
}
time_it <- system.time({
  pso <- psoptim(
    par = rep(1/ncol(mat$Dmat), ncol(mat$Dmat)),
    fn = pso_fn,
    lower = 0,
    upper = 1,
    control = list(
      trace = 1,
      maxit = 50,
      s = 200,
      p = 1
    )
  )
})


pso_port_returns_train <- xts(asset_returns_train %*% pso$par, order.by = index(asset_returns_train)) %>% `colnames<-`(., "pso_port_returns_train")
pso_port_returns_test <- xts(asset_returns_test %*% pso$par, order.by = index(asset_returns_test)) %>% `colnames<-`(., "pso_port_returns_test")

pso_alpha_train <- ret_to_cumret(pso_port_returns_train) - ret_to_cumret(bm_returns_train)
pso_alpha_test <- ret_to_cumret(pso_port_returns_test) - ret_to_cumret(bm_returns_test)

info <- bind_rows(
  info,
  data.frame(
    "date" = date,
    "optimizer" = "PSO",
    "objectiv_fitness" = objectiv_fitness(pso$par, mat),
    "constraint_check" = constraint_check(pso$par, mat),
    "real_MSE_train" = MSE(pso_alpha_train),
    "real_MSE_test" = MSE(pso_alpha_test),
    "raw_MSE_train" = MSE(pso_port_returns_train - bm_returns_train),
    "raw_MSE_test" = MSE(pso_port_returns_test - bm_returns_test),
    "time" = time_it[3]
  )
)

##########################################
# PSO with MSE objectiv
##########################################

pso_fn_mse = function(x){
  fit <- objectiv_fitness(x, mat)
  constraint <- constraint_check(x, mat)
  mse <-  MSE(ret_to_cumret(xts(asset_returns_train %*% x, order.by=index(asset_returns_train))) - ret_to_cumret(bm_returns_train))

  return(fit + constraint + 0.01 * mse)
}

time_it <- system.time({
  pso_mse <- psoptim(
    par = rep(1/ncol(mat$Dmat), ncol(mat$Dmat)),
    fn = pso_fn_mse,
    lower = 0,
    upper = 1,
    control = list(
      trace = 1,
      maxit = 50,
      s = 200,
      p = 1
    )
  )
})

pso_mse_port_returns_train <- xts(asset_returns_train %*% pso_mse$par, order.by = index(asset_returns_train)) %>% `colnames<-`(., "pso_mse_port_returns_train")
pso_mse_port_returns_test <- xts(asset_returns_test %*% pso_mse$par, order.by = index(asset_returns_test)) %>% `colnames<-`(., "pso_mse_port_returns_test")

pso_mse_alpha_train <- ret_to_cumret(pso_mse_port_returns_train) - ret_to_cumret(bm_returns_train)
pso_mse_alpha_test <- ret_to_cumret(pso_mse_port_returns_test) - ret_to_cumret(bm_returns_test)

info <- bind_rows(
  info,
  data.frame(
    "date" = date,
    "optimizer" = "PSO_MSE",
    "objectiv_fitness" = objectiv_fitness(pso_mse$par, mat),
    "constraint_check" = constraint_check(pso_mse$par, mat),
    "real_MSE_train" = MSE(pso_mse_alpha_train),
    "real_MSE_test" = MSE(pso_mse_alpha_test),
    "raw_MSE_train" = MSE(pso_mse_port_returns_train - bm_returns_train),
    "raw_MSE_test" = MSE(pso_mse_port_returns_test - bm_returns_test),
    "time" = time_it[3]
  )
)


##########################################
# Visualize
##########################################

rownames(info) <- NULL
info

plotly_line_chart_xts(ret_to_cumret(cbind.xts(qp_port_returns_train, pso_port_returns_train, pso_mse_port_returns_train, bm_returns_train))) %>% layout(title="train")
plotly_line_chart_xts(ret_to_cumret(cbind.xts(qp_port_returns_test, pso_port_returns_test, pso_mse_port_returns_test, bm_returns_test))) %>% layout(title="test")









