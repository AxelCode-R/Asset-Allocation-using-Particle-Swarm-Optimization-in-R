


cov_ <- function(mat, mean_vec=NULL){
  if(is.null(mean_vec)){
    mat_mean <- matrix(data=1, nrow=nrow(mat)) %*% apply(mat, 2, mean)
  }else{
    mat_mean <- matrix(data=1, nrow=nrow(mat)) %*% mean_vec
  }
  mat <- mat - mat_mean

  return((nrow(mat)-1)^(-1) * t(mat) %*% mat)
}




returns <- buffer(
  get_yf(tickers = c("IBM", "GOOG", "AAPL"), from = "2018-01-01", to = "2019-12-31")$returns,
  "CPI_3_assets"
)

# save geo to compare results
mu_arit <- apply(returns, 2, mean) #ret_to_geomeanret(returns)
cov_arit <- cov(returns)

mu <- ret_to_geomeanret(returns) #apply(returns, 2, mean) #ret_to_geomeanret(returns)
cov <- cov_(returns, mean_vec = mu)


portfolios <- data.frame()
mu_and_var <- NULL
for(lambda in seq(0.01,1, 0.01)){
  mat <- list(
    Dmat = lambda * cov,
    dvec = (1 - lambda) * mu,
    Amat = t(rbind(
      rep(1, ncol(returns)), # sum up to 1
      diag(1, nrow=ncol(returns), ncol=ncol(returns)) # long only
    )),
    bvec = c(
      1, # sum up to 1
      rep(0, ncol(returns)) # long only
    ),
    meq = 1
  )

  qp <- solve.QP(Dmat = mat$Dmat, dvec = mat$dvec, Amat = mat$Amat, bvec = mat$bvec, meq = mat$meq)

  port <- xts(returns %*% qp$solution, order.by=index(returns))

  mu_and_var <- rbind(
    mu_and_var,
    data.frame("lambda" = lambda, "mu" = mu_arit %*% qp$solution, "sd" = sqrt(t(qp$solution) %*% cov_arit %*% qp$solution))
  )
  portfolios <- rbind(
    portfolios,
    qp$solution
  )
}
portfolios <- data.frame(portfolios)
colnames(portfolios) <- colnames(returns)

all <- data.frame(type="geo_mean", mu_and_var)



mu <- ret_to_geomeanret(returns) #apply(returns, 2, mean) #ret_to_geomeanret(returns)
cov <- cov(returns)


portfolios <- data.frame()
mu_and_var <- NULL
for(lambda in seq(0.01,1, 0.01)){
  mat <- list(
    Dmat = lambda * cov,
    dvec = (1 - lambda) * mu,
    Amat = t(rbind(
      rep(1, ncol(returns)), # sum up to 1
      diag(1, nrow=ncol(returns), ncol=ncol(returns)) # long only
    )),
    bvec = c(
      1, # sum up to 1
      rep(0, ncol(returns)) # long only
    ),
    meq = 1
  )

  qp <- solve.QP(Dmat = mat$Dmat, dvec = mat$dvec, Amat = mat$Amat, bvec = mat$bvec, meq = mat$meq)

  port <- xts(returns %*% qp$solution, order.by=index(returns))

  mu_and_var <- rbind(
    mu_and_var,
    data.frame("lambda" = lambda, "mu" = mu_arit %*% qp$solution, "sd" = sqrt(t(qp$solution) %*% cov_arit %*% qp$solution))
  )
  portfolios <- rbind(
    portfolios,
    qp$solution
  )
}
portfolios <- data.frame(portfolios)
colnames(portfolios) <- colnames(returns)

all <- rbind(all, data.frame(type="geo_mean_and_cov_with_arith", mu_and_var))



mu <- apply(returns, 2, mean)
cov <- cov(returns)



portfolios <- data.frame()
mu_and_var <- NULL
for(lambda in seq(0.01,1, 0.01)){
  mat <- list(
    Dmat = lambda * cov,
    dvec = (1 - lambda) * mu,
    Amat = t(rbind(
      rep(1, ncol(returns)), # sum up to 1
      diag(1, nrow=ncol(returns), ncol=ncol(returns)) # long only
    )),
    bvec = c(
      1, # sum up to 1
      rep(0, ncol(returns)) # long only
    ),
    meq = 1
  )

  qp <- solve.QP(Dmat = mat$Dmat, dvec = mat$dvec, Amat = mat$Amat, bvec = mat$bvec, meq = mat$meq)

  port <- xts(returns %*% qp$solution, order.by=index(returns))

  mu_and_var <- rbind(
    mu_and_var,
    data.frame("lambda" = lambda, "mu" = mu_arit %*% qp$solution, "sd" = sqrt(t(qp$solution) %*% cov_arit %*% qp$solution))
  )
  portfolios <- rbind(
    portfolios,
    qp$solution
  )
}
portfolios <- data.frame(portfolios)
colnames(portfolios) <- colnames(returns)

all <- rbind(all, data.frame(type="arith_mean", mu_and_var))



plot_ly() %>%
  add_lines(data = all %>% filter(type=="geo_mean"), y = ~mu, x = ~sd, name="geo_mean", mode="lines", type = 'scatter') %>%
  add_trace(data = all %>% filter(type=="geo_mean"), x = ~sd, y=~mu, name="geo_mean", mode="markers", type = 'scatter') %>%
  add_lines(data = all %>% filter(type=="arith_mean"), y = ~mu, x = ~sd, name="arith_mean", mode="lines", type = 'scatter') %>%
  add_trace(data = all %>% filter(type=="arith_mean"), x = ~sd, y=~mu, name="arith_mean", mode="markers", type = 'scatter') %>%
  add_lines(data = all %>% filter(type=="geo_mean_and_cov_with_arith"), y = ~mu, x = ~sd, name="geo_mean_and_cov_with_arith", mode="lines", type = 'scatter') %>%
  add_trace(data = all %>% filter(type=="geo_mean_and_cov_with_arith"), x = ~sd, y=~mu, name="geo_mean_and_cov_with_arith", mode="markers", type = 'scatter') %>%
  layout(
    title = "3-Asset MVP: arith_mean or geo_mean",
    yaxis = list(range=c(min(mu_and_var$mu)*0.9, max(mu_and_var$mu)*1.1)),
    xaxis = list(range=c(min(mu_and_var$sd)*0.95, max(mu_and_var$sd)*1.05)),
    margin = list(
      l = 10,
      r = 10,
      b = 70,
      t = 50,
      pad = 4
    )
  ) %>%
  html_save()


