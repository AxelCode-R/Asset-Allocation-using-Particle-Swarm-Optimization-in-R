source("global.R")
library(metaheuristicOpt)



nav <- 20000

from <- "2018-01-01"
to <- "2019-12-31"

spx_composition <- buffer(
  get_spx_composition(),
  "AS_spx_composition"
)


pool_data <- buffer(
  get_yf(
    tickers = spx_composition %>%
      filter(Date<=to) %>%
      filter(Date==max(Date)) %>%
      pull(Ticker),
    from = from,
    to = to
  ),
  "AS_sp500_asset_data"
)

load("data/assets_pool_50.rdata")

pool_data$returns <- pool_data$returns[, assets_pool_50]
pool_data$prices <- pool_data$prices[, assets_pool_50]


bm_returns <- buffer(
  get_yf(tickers = "^SP500TR", from = from, to = to)$returns,
  "AS_sp500tr"
) %>% setNames(., "SP500TR")


pool_returns <- pool_data$returns
mat <- list(
  Dmat = t(pool_returns) %*% pool_returns,
  dvec = t(pool_returns) %*% bm_returns,
  Amat = t(rbind(
    -rep(1, ncol(pool_returns)), # sum w <= 1
    rep(1, ncol(pool_returns)), # sum w >= 0.99
    diag(1,
         nrow=ncol(pool_returns),
         ncol=ncol(pool_returns)) # long only
  )),
  bvec = c(
    -1, # sum w <= 1
    0.99, # sum w >= 0.99
    rep(0, ncol(pool_returns)) # long only
  ),
  meq = 0
)

prices <- last(pool_data$prices)

calc_fit <- function(x){
  as.numeric(0.5 * t(x) %*% mat$Dmat %*% x - t(mat$dvec) %*% x)
}
calc_const <- function(x){
  const <- t(mat$Amat) %*% x - mat$bvec
  sum(pmin(0, const)^2)
}

set.seed(0)



res_SPSO_time <- system.time({
  res_SPSO <- pso(
    par = rep(0, ncol(pool_data$returns)),
    fn = function(x){
      x <- as.vector(round(x*nav/prices)*prices/nav)
      fitness <- calc_fit(x)
      constraints <- calc_const(x)
      return(fitness+100*constraints)
    },
    lower = 0,
    upper = 0.1,
    control = list(
      s = 40, # swarm size
      c.p = 0.5, # inherit best
      c.g = 0.5, # global best
      maxiter = 400, # iterations
      w0 = 1.2, # starting inertia weight
      wN = 0, # ending inertia weight
      save_fit = T # save more information
    )
  )
})
res_SPSO$solution <- as.vector(round(res_SPSO$solution*nav/prices)*prices/nav)
res_SPSO$fitness <- calc_fit(res_SPSO$solution)
res_SPSO$constraints <- calc_const(res_SPSO$solution)


df_PSOs <- NULL
load("data/save_variant1.rdata")
df_PSOs <- rbind(df_PSOs, df_res %>% filter(type=="PSO", iter==400))
load("data/save_variant2.rdata")
df_PSOs <- rbind(df_PSOs, df_res %>% filter(type!="PSO", iter==400))
load("data/save_variant3.rdata")
df_PSOs <- rbind(df_PSOs, df_res %>% filter(type!="PSO", iter==400))
load("data/save_variant5.rdata")
df_PSOs <- rbind(df_PSOs, df_res %>% filter(type!="PSO", iter==400))
load("data/save_variant6.rdata")
df_PSOs <- rbind(df_PSOs, df_res %>% filter(type!="PSO", iter==400))


methaheuristics <- c("ABC", "ALO", "BA", "BHO", "CLONALG", "CS", "CSO", "DA", "DE", "FFA", "GA", "GOA", "GWO", "HS", "KH", "MFO", "SCA", "SFL", "WOA")
n_tests <- 1

res_all <- NULL
for(i in 1:length(methaheuristics)){
  opt_name <- methaheuristics[i]
  try({
    for(k in 1:n_tests){
      eval(
        expr=parse(
          text=paste('
            time <- system.time({
              res <- ',opt_name,'(
                FUN = function(x){
                  x <- as.vector(round(x*nav/prices)*prices/nav)
                  fitness <- calc_fit(x)
                  constraints <- calc_const(x)
                  return(fitness+100*constraints)
                },
                optimType = "MIN",
                numVar = ncol(pool_data$returns),
                numPopulation = 40,
                maxIter = 400,
                rangeVar = matrix(c(0, 0.1), ncol=ncol(pool_data$returns), nrow=2)
              )
            }) %>% .[3]
            res <- as.vector(round(unlist(res)*nav/prices)*prices/nav)
            res_df <- data.frame(
              opt_name = opt_name,
              fitness = calc_fit(res),
              constraints = calc_const(res),
              time = time,
              row.names = NULL
            )
          '))
      )


      res_all <- rbind(res_all, res_df)
    }
  })
}

res_all$overall_fit <- res_all$fitness+res_all$constraints





