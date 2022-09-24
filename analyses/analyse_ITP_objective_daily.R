source("global.R")

from <- "2018-01-01"
to <- "2019-12-31"

spx_composition <- buffer(
  get_spx_composition(),
  "AS_spx_composition"
)



pool_returns <- buffer(
  get_yf(
    tickers = spx_composition %>%
      filter(Date<=to) %>%
      filter(Date==max(Date)) %>%
      pull(Ticker),
    from = from,
    to = to
  )$returns,
  "AS_sp500_assets"
)
pool_returns <-
  pool_returns[, colSums(is.na(pool_returns))==0]


bm_returns <- buffer(
  get_yf(tickers = "%5EGSPC", from = from, to = to)$returns,
  "AS_sp500"
) %>% setNames(., "S&P 500")




res <- list()


train_days <- 90
test_days <- 1
dates <- index(pool_returns) #[index(pool_returns) >= min(index(pool_returns))+days(train_period) & index(pool_returns) <= max(index(pool_returns))-days(test_period)]

# Portfolio wird am morgen erstellt um 6 uhr und gekauft
for(i in (1+train_days):(length(dates)-test_days)){

  train_interval <- paste0(dates[i-train_days], "/", dates[i]-1)
  test_interval <- paste0(dates[i], "/", dates[i-1+test_days])

  print(paste0("train_interval: ", train_interval, "    test_interval: ", test_interval))

  # VAR(TE) -> min
  mat <- list(
    Dmat = cov(pool_returns[train_interval, ]),
    dvec = cov(pool_returns[train_interval, ], bm_returns[train_interval, ]),
    Amat = t(rbind(
      rep(1, ncol(pool_returns)), # sum up to 1
      diag(1,
           nrow=ncol(pool_returns),
           ncol=ncol(pool_returns)) # long only
    )),
    bvec = c(
      1, # sum up to 1
      rep(0, ncol(pool_returns)) # long only
    ),
    meq = 1
  )
  if(!is.positive.definite(mat$Dmat)){
    mat$Dmat <- as.matrix(nearPD(mat$Dmat)$mat)
  }

  qp <- solve.QP(
    Dmat = mat$Dmat, dvec = mat$dvec,
    Amat = mat$Amat, bvec = mat$bvec, meq = mat$meq
  )

  res$QP_VAR <- list(
    "test_perf" = rbind(res$QP_VAR$test_perf, calc_portfolio_returns(pool_returns[test_interval, ], qp$solution))
  )


  # MSE(TE) -> min
  mat <- list(
    Dmat = t(pool_returns[train_interval, ]) %*% pool_returns[train_interval, ],
    dvec = as.vector(t(pool_returns[train_interval, ]) %*% bm_returns[train_interval, ]),
    Amat = t(rbind(
      rep(1, ncol(pool_returns)), # sum up to 1
      diag(1,
           nrow=ncol(pool_returns),
           ncol=ncol(pool_returns)) # long only
    )),
    bvec = c(
      1, # sum up to 1
      rep(0, ncol(pool_returns)) # long only
    ),
    meq = 1
  )

  if(!is.positive.definite(mat$Dmat)){
    mat$Dmat <- as.matrix(nearPD(mat$Dmat)$mat)
  }

  qp <- solve.QP(
    Dmat = mat$Dmat, dvec = mat$dvec,
    Amat = mat$Amat, bvec = mat$bvec, meq = mat$meq
  )

  res$QP_TE <- list(
    "test_perf" = rbind(res$QP_TE$test_perf, calc_portfolio_returns(pool_returns[test_interval, ], qp$solution))
  )


  # PSO with constraints
  pso <- psoptim(
    par = rep(0, ncol(pool_returns)),
    fn = function(x){
      #x = x/sum(x)
      fit <- sqrt(sum((ret_to_cumret(xts(pool_returns %*% x, order.by=index(pool_returns))) - ret_to_cumret(bm_returns))^2))
      sum_wgt <- max(abs(sum(x)-0.99)-0.01, 0)
      return(fit + 10 * sum_wgt)
    },
    lower = 0,
    upper = 0.1,
    control = list(
      trace = F,
      s = 100,
      maxit = 200
    )
  )

  res$PSO_CONST <- list(
    "test_perf" = rbind(res$PSO_CONST$test_perf, calc_portfolio_returns(pool_returns[test_interval, ], pso$par))
  )


  # PSO with transformation of positions
  pso <- psoptim(
    par = rep(0, ncol(pool_returns)),
    fn = function(x){
      x <- if(sum(x)!=0){
        x/sum(x)
      }else{
        x
      }
      fit <- sqrt(sum((ret_to_cumret(xts(pool_returns %*% x, order.by=index(pool_returns))) - ret_to_cumret(bm_returns))^2))
      return(fit)
    },
    lower = 0,
    upper = 0.1,
    control = list(
      trace = F,
      s = 100,
      maxit = 200
    )
  )
  pso$par <- pso$par/sum(pso$par)

  res$PSO_TRANS <- list(
    "test_perf" = rbind(res$PSO_TRANS$test_perf, calc_portfolio_returns(pool_returns[test_interval, ], pso$par))
  )

}

all_test_perfs <- cbind.xts(
  bm_returns[paste0(dates[train_days+1], "/", dates[length(dates)]), ],
  setNames(res$QP_VAR$test_perf, "QP_VAR"),
  setNames(res$QP_TE$test_perf, "QP_TE"),
  setNames(res$PSO_CONST$test_perf, "PSO_CONST"),
  setNames(res$PSO_TRANS$test_perf, "PSO_TRANS")
)

shapes <- lapply(dates[(train_days+1):length(dates)], function(x){
  list(
    type="line",
    xref="x",
    yref="paper",
    x0=x,
    x1=x,
    y0=0,
    y1=1,
    line = list(color="lightgrey"),
    opacity = 0.9,
    layer='below'
  )
})

plotly_line_chart_xts(ret_to_cumret(all_test_perfs)) %>%
  layout(shapes=shapes, yaxis=list(showgrid=F), xaxis=list(showgrid=F))

#save.image("analyses/save_ITP_objective_daily_v2.rdata")






# SPDR S&P 500 ETF Trust (SPY)
# Invesco S&P 500 UCITS ETF (SPXS.MI)
# Lyxor S&P 500 UCITS ETF - D-EUR (SPX.MI)
# Vanguard S&P 500 UCITS ETF (VUSA.DE)
etf_returns <- get_yf(tickers = c("%5EGSPC" ,"SPY", "SPXS.MI", "SPX.MI", "VUSA.DE"), from = from, to = to)$returns
all_bms <- cbind.xts(all_test_perfs, etf_returns[paste0(min(index(all_test_perfs)), "/", max(index(all_test_perfs))),])
all_bms[is.na(all_bms)] <- 0
all_bms <- all_bms - matrix(rep(coredata(all_bms$S.P.500), ncol(all_bms)), ncol=ncol(all_bms), byrow = F)

plotly_line_chart_xts(ret_to_cumret(all_bms)) %>%
  layout(shapes=shapes, yaxis=list(showgrid=F), xaxis=list(showgrid=F))










# Compare ETFs and the S&P 500

plotly_line_chart_xts(ret_to_cumret(cbind.xts(all_test_perfs, all_bms[paste0(min(index(all_test_perfs)), "/", max(index(all_test_perfs)))]))) %>%
  layout(shapes=shapes, yaxis=list(showgrid=F), xaxis=list(showgrid=F))


# SPDR S&P 500 ETF Trust (SPY)
# Invesco S&P 500 UCITS ETF (SPXS.MI)
# Lyxor S&P 500 UCITS ETF - D-EUR (SPX.MI)
# Vanguard S&P 500 UCITS ETF (VUSA.DE)
etf_returns <- get_yf(tickers = c("%5EGSPC" ,"SPY"), from = from, to = to)$returns
all_bms <- cbind.xts(all_test_perfs, etf_returns[paste0(min(index(all_test_perfs)), "/", max(index(all_test_perfs))),])
all_bms[is.na(all_bms)] <- 0
all_bms <- all_bms - matrix(rep(coredata(all_bms$S.P.500), ncol(all_bms)), ncol=ncol(all_bms), byrow = F)

plotly_line_chart_xts(ret_to_cumret(all_bms)) %>%
  layout(shapes=shapes, yaxis=list(showgrid=F), xaxis=list(showgrid=F))

