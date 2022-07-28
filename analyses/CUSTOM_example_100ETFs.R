source("global.R")

# load("data/etf_daily_adj_data.rdata")
#
#
# # reduce to most uncorrolated
# uncorr_ticker <- ret_to_cumret(etf_daily_adj_data$daily_returns) %>%
#   correlate() %>%
#   stretch() %>%
#   arrange(r) %>%
#   filter(x %in% first(unique(.$x),5)) %>%
#   {c(.$x)} %>%
#   unlist() %>%
#   unique()
#
# etf_daily_adj_data$daily_returns <- etf_daily_adj_data$daily_returns[, uncorr_ticker]
# etf_daily_adj_data$daily_prices <- etf_daily_adj_data$daily_prices[, uncorr_ticker]


load("C:/Users/Axel/Desktop/Master-Thesis-All/Master-Thesis/data/spx_index_daily_adj_data.rdata")
load("C:/Users/Axel/Desktop/Master-Thesis-All/Master-Thesis/data/spx_daily_adj_data.rdata")
load("C:/Users/Axel/Desktop/Master-Thesis-All/Master-Thesis/data/spx_composition.rdata")





# formeln: tr2_16.pdf
train_months <- 6
test_months <- 3
date <- as.Date("2017-02-01")

valid_tickers <- spx_composition %>%
  filter(Date <= date) %>% filter(Date == max(Date)) %>% pull(Ticker) %>% .[. %in% colnames(spx_daily_adj_data$daily_returns)]
etf_daily_adj_data <- list()
etf_daily_adj_data$daily_returns <- spx_daily_adj_data$daily_returns[,valid_tickers]
etf_daily_adj_data$daily_prices <- spx_daily_adj_data$daily_prices[,valid_tickers]

train_period_xts <- paste0(date-months(train_months), "/", date-days(1))
test_period_xts <- paste0(date, "/", date + months(test_months))

asset_returns_train <- etf_daily_adj_data$daily_returns[train_period_xts, ] %>% .[,colSums(is.na(.))==0]
asset_returns_test <- etf_daily_adj_data$daily_returns[test_period_xts, colnames(asset_returns_train)] %>% .[,colSums(is.na(.))==0]
if(ncol(asset_returns_train) != ncol(asset_returns_test)){ message("Assets in train and test are diffrent!") }

asset_prices_train <- etf_daily_adj_data$daily_prices[train_period_xts, ] %>% .[,colSums(is.na(.))==0]
asset_mean_ret <- (last(asset_prices_train)/first(coredata(asset_prices_train)))^(1/(nrow(asset_prices_train)-1))-1

# file:///C:/Users/Axel/Desktop/Master-Thesis-All/Ziel%20was%20beantwortet%20werden%20soll/Quellen%20nur%20wichtige/Quantitative%20Equity%20Investing%20Techniques%20and%20Strategies%20(The%20Frank%20J.%20Fabozzi%20Series)%20by%20Frank%20J.%20Fabozzi%20CFA,%20Sergio%20M.%20Focardi,%20Petter%20N.%20Kolm).pdf
max_wgt <- 1
risk_aversion <- 1
mat <- list(
  Dmat = risk_aversion * cov(asset_returns_train, asset_returns_train),
  dvec = (1 - risk_aversion) * as.vector(asset_mean_ret),
  Amat = t(rbind(
    rep(1, ncol(asset_returns_train)), # sum up to 1
    diag(1, nrow=ncol(asset_returns_train), ncol=ncol(asset_returns_train)), # long only
    -diag(1, nrow=ncol(asset_returns_train), ncol=ncol(asset_returns_train)) # max_wgt
  )),
  bvec = c(
    1, # sum up to 1
    rep(0, ncol(asset_returns_train)), # long only
    -rep(max_wgt, ncol(asset_returns_train))
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
  temp <- t(mat$Amat) %*% x - mat$bvec

  # cause meq = 1
  temp[1,] <- -abs(temp[1,])

  as.numeric(- sum(pmin(0, temp)))
}




# info_all <- NULL
# for(i in 1:10){

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


  qp_port_returns_train <- xts(asset_returns_train %*% round(qp$solution, 8), order.by = index(asset_returns_train)) %>% `colnames<-`(., "qp_port_returns_train")
  qp_port_returns_test <- xts(asset_returns_test %*% round(qp$solution, 8), order.by = index(asset_returns_test)) %>% `colnames<-`(., "qp_port_returns_test")


  info <- bind_rows(
    info,
    data.frame(
      "date" = date,
      "optimizer" = "QP",
      "objectiv_fitness" = objectiv_fitness(qp$solution, mat),
      "constraint_check" = constraint_check(qp$solution, mat),
      "var_train" = var(qp_port_returns_train),
      "var_test" = var(qp_port_returns_test),
      "var_real_train" = var(ret_to_cumret(qp_port_returns_train)),
      "var_real_test" = var(ret_to_cumret(qp_port_returns_test)),
      "mu_train" = coredata(last(ret_to_cumret(qp_port_returns_train)))/coredata(first(ret_to_cumret(qp_port_returns_train)))-1,
      "mu_test" = coredata(last(ret_to_cumret(qp_port_returns_test)))/coredata(first(ret_to_cumret(qp_port_returns_test)))-1,
      "time" = time_it[3]
    )
  )

  ##########################################
  # PSO
  ##########################################

  pso_fn = function(x){
    x = x/sum(x)
    fit <- objectiv_fitness(x, mat)
    constraint <- constraint_check(x, mat)

    return(fit + constraint)
  }
  time_it <- system.time({
    save_lsg <- NULL
    save_fit <- NULL
    for(i in 1:2){
      pso <- psoptim(
        par = rep(NA, ncol(mat$Dmat)),
        fn = pso_fn,
        lower = 0,
        upper = max_wgt,
        control = list(
          trace = 1,
          maxit = 50,
          s = 200,
          p = 1
        )
      )
      save_lsg <- rbind(save_lsg, pso$par)
      save_fit <- c(save_fit, pso$value)
    }
    pso <- psoptim(
      par = save_lsg[which(save_fit == min(save_fit))[1], ],
      fn = pso_fn,
      lower = 0,
      upper = max_wgt,
      control = list(
        trace = 1,
        trace.stats = T,
        maxit = 100,
        s = 200,
        p = 1
      )
    )
  })


  pso$par <- pso$par/sum(pso$par)
  pso_port_returns_train <- xts(asset_returns_train %*% round(pso$par, 8), order.by = index(asset_returns_train)) %>% `colnames<-`(., "pso_port_returns_train")
  pso_port_returns_test <- xts(asset_returns_test %*% round(pso$par, 8), order.by = index(asset_returns_test)) %>% `colnames<-`(., "pso_port_returns_test")


  info <- bind_rows(
    info,
    data.frame(
      "date" = date,
      "optimizer" = "PSO",
      "objectiv_fitness" = objectiv_fitness(pso$par, mat),
      "constraint_check" = constraint_check(pso$par, mat),
      "var_train" = var(pso_port_returns_train),
      "var_test" = var(pso_port_returns_test),
      "var_real_train" = var(ret_to_cumret(pso_port_returns_train)),
      "var_real_test" = var(ret_to_cumret(pso_port_returns_test)),
      "mu_train" = coredata(last(ret_to_cumret(pso_port_returns_train)))/coredata(first(ret_to_cumret(pso_port_returns_train)))-1,
      "mu_test" = coredata(last(ret_to_cumret(pso_port_returns_test)))/coredata(first(ret_to_cumret(pso_port_returns_test)))-1,
      "time" = time_it[3]
    )
  )


  rownames(info) <- NULL
  info


#   info_all <- bind_rows(info_all, info)
# }
# #save(info_all, file="analyses/info_all.rdata")
# info_all %>% group_by(optimizer) %>% summarise_all(., function(x){if(is.numeric(x)){quantile(x, 0.5)}else{unique(x)}})
# info_all %>% group_by(optimizer) %>% summarise_all(., function(x){if(is.numeric(x)){quantile(x, 0.9)}else{unique(x)}})
# info_all %>% group_by(optimizer) %>% summarise_all(., function(x){if(is.numeric(x)){quantile(x, 0.1)}else{unique(x)}})

plotly_line_chart_xts(ret_to_cumret(cbind.xts(qp_port_returns_train, pso_port_returns_train))) %>% layout(title="train")

plotly_line_chart_xts(ret_to_cumret(cbind.xts(qp_port_returns_test, pso_port_returns_test))) %>% layout(title="test")


# p1 <- plot_ly(data = info_all, type = "box") %>% add_trace(y = ~real_MSE_train, name=~optimizer) %>% layout(yaxis=list(title="real_MSE_train", range=c(0,max(info_all$real_MSE_train)*1.1)), showlegend=FALSE)
# p2 <- plot_ly(data = info_all, type = "box") %>% add_trace(y = ~real_MSE_test, name=~optimizer) %>% layout(yaxis=list(title="real_MSE_test", range=c(0,max(info_all$real_MSE_test)*1.1)), showlegend=FALSE)
# p3 <- plot_ly(data = info_all, type = "box") %>% add_trace(y = ~raw_MSE_train, name=~optimizer) %>% layout(yaxis=list(title="raw_MSE_train", range=c(0,max(info_all$raw_MSE_train)*1.1)), showlegend=FALSE)
# p4 <- plot_ly(data = info_all, type = "box") %>% add_trace(y = ~raw_MSE_test, name=~optimizer) %>% layout(yaxis=list(title="raw_MSE_test", range=c(0,max(info_all$raw_MSE_test)*1.1)), showlegend=FALSE)
#
# subplot(
#   p1,
#   p2,
#   p3,
#   p4,
#   nrows = 2,
#   titleY = T
# )





