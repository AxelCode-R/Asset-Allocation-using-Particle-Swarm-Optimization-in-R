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

system.time({
  for(i in 1:20000){
    x <- runif(50)
    fit <- calc_fit(x)
    const <- calc_const(x)
  }
})

df_PSOs <- NULL
load("data/save_variant1.rdata")
df_PSOs <- bind_rows(df_PSOs, df_SPSO %>% filter(type=="PSO", iter==400))
load("data/save_variant2.rdata")
df_PSOs <- bind_rows(df_PSOs, df %>% filter(type!="PSO", iter==400))
load("data/save_variant3.rdata")
df_PSOs <- bind_rows(df_PSOs, df %>% filter(type!="PSO", iter==400))
load("data/save_variant5.rdata")
df_PSOs <- bind_rows(df_PSOs, df %>% filter(type!="PSO", iter==400))
load("data/save_variant6.rdata")
df_PSOs <- bind_rows(df_PSOs, df %>% filter(type!="PSO", iter==400))
df_PSOs <- df_PSOs %>% select(type, time, fitness=best_fit, const_break) %>% mutate(created=1)




plot_ly(data=df_PSOs, x = ~fitness, type = "box", boxpoints="all", y = ~type, colors = ~created)



methaheuristics <- c("ABC", "ALO", "BA", "BHO", "CLONALG", "CS", "CSO", "DA", "DE", "FFA", "GA", "GOA", "GWO", "HS", "KH", "MFO", "SCA", "WOA")
n_tests <- 100

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



#save(res_all, file="data/external_methaheuristics.rdata")

load("data/external_methaheuristics.rdata")




df_PSOs <- NULL
load("data/save_variant1.rdata")
df_PSOs <- bind_rows(df_PSOs, df_SPSO %>% filter(type=="PSO", iter==400))
load("data/save_variant2.rdata")
df_PSOs <- bind_rows(df_PSOs, df %>% filter(type!="PSO", iter==400))
load("data/save_variant3.rdata")
df_PSOs <- bind_rows(df_PSOs, df %>% filter(type!="PSO", iter==400))
load("data/save_variant5.rdata")
df_PSOs <- bind_rows(df_PSOs, df %>% filter(type!="PSO", iter==400))
load("data/save_variant6.rdata")
df_PSOs <- bind_rows(df_PSOs, df %>% filter(type!="PSO", iter==400))
df_PSOs <- df_PSOs %>% select(type, time, fitness=best_fit, const_break) %>% mutate(created=1)

res_all <- bind_rows(
  df_PSOs,
  res_all %>% mutate(created=0, fitness = overall_fit) %>% select(type = opt_name, fitness, "const_break"=constraints, time, created)
)
#   \text{score} = 2 \cdot \text{q_5%_fit_rnk} + \text{mean_fit_rnk} + \text{q_95%_fit_rnk} + 0.25 \cdot \text{q_5%_const_break_rnk} +
#0.25 \cdot \text{mean_const_break_rnk} + 0.5 \cdot \text{q_95%_const_break_rnk} + 2 \cdot \text{mean_runtime_rnk}

res_all_agg <- res_all %>%
  group_by(type) %>%
  summarise(
    created = unique(created),
    fitness_q5 = quantile(fitness, 0.05),
    fitness_mean = mean(fitness),
    fitness_q95 = quantile(fitness, 0.95),
    const_break_q5 = quantile(const_break, 0.05),
    const_break_mean = mean(const_break),
    const_break_q95 = quantile(const_break, 0.95),
    time_mean = mean(time)
  ) %>%
  mutate(
    score = 3 * rank(fitness_q5) + 2 * rank(fitness_mean) + rank(fitness_q95) +
      1 * rank(const_break_q5) + 0.5 * rank(const_break_mean) + 0.5 * rank(const_break_q95) +
      3 * rank(time_mean)
  ) %>%
  arrange(score)
#res_all_agg$type <- factor(res_all_agg$type, levels = res_all_agg$type)


p1 <- plot_ly(data=res_all %>% filter(!type %in% c("FFA")), x = ~fitness, type = "box", boxpoints="all", y = ~type) %>%
  layout(xaxis = list(range=c(min(res_all$fitness)-0.00005,-0.021)), yaxis = list(title=list(text="metaheuristic", standoff = 1), categoryorder = "array", categoryarray = rev(res_all_agg$type)))

p2 <- plot_ly(data=res_all %>% filter(!type %in% c("FFA")), x = ~const_break, type = "box", boxpoints="all", y = ~type) %>%
  layout(xaxis = list(range=c(0,0.5)), yaxis = list(standoff = 40, title="metaheuristic", categoryorder = "array", categoryarray = rev(res_all_agg$type)))

p3 <- plot_ly(data=res_all %>% filter(!type %in% c("FFA")), x = ~time, type = "box", boxpoints="all", y = ~type) %>%
  layout(xaxis = list(range=c(0,30)), yaxis = list(standoff = 40, title="metaheuristic", categoryorder = "array", categoryarray = rev(res_all_agg$type)), showlegend=F)

p <- subplot(p1, p2, p3, nrows = 1, shareY = T, titleX=T)

p





