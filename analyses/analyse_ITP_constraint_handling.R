
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
pool_returns <- pool_returns[, colSums(is.na(pool_returns))==0]


bm_returns <- buffer(
  get_yf(tickers = "%5EGSPC", from = from, to = to)$returns,
  "AS_sp500"
) %>% setNames(., "S&P 500")




mat <- list(
  Dmat = t(pool_returns) %*% pool_returns,
  dvec = t(pool_returns) %*% bm_returns,
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

calc_fit <- function(x){
  as.numeric(0.5 * t(x) %*% mat$Dmat %*% x - t(mat$dvec) %*% x)
}
calc_const <- function(x){
  const <- t(mat$Amat) %*% x - mat$bvec
  const[mat$meq] <- -max(0, abs(const[mat$meq]+0.005)-0.005)
  res <- sum(pmin(0, const)^2)
  if(res>0){
    res #+ 1
  }else{
    res
  }
}


pso_res <- pso(
  par = rep(0, ncol(pool_returns)),
  fn = function(x){
    fitness <- calc_fit(x)
    constraints <- calc_const(x)
    return(fitness+5*constraints)
  },
  lower = 0,
  upper = 0.05,
  control = list(
    s = 100, # swarm size
    c.p = 0.5, # inherit best
    c.g = 0.5, # global best
    maxiter = 50, # iterations
    w0 = 1.2, # starting inertia weight
    wN = 0, # ending inertia weight
    save_traces = F, # save more information
    save_fit = T
  )
)

plot_ly(data=pso_res$fit_data, mode="lines", type = 'scatter') %>%
  add_trace(x=~iter, y=~mean, name="mean") %>%
  add_trace(x=~iter, y=~best, name="best") %>%
  layout(yaxis=list(range=c(-0.1, 10)))


#
# res <- psoptim(
#   par = rep(0, ncol(pool_returns)),
#   fn = function(x){
#     fitness <- calc_fit(x)
#     constraints <- calc_const(x)
#     return(fitness+5*constraints)
#   },
#   lower = 0,
#   upper = 0.05,
#   control = list(
#     s = 100,
#     maxit = 100
#   )
# )




# default PSO
pso_default2 <- function(
    par,
    fn,
    lower,
    upper,
    control = list()
){

  # use default control values if not set
  control_ = list(
    s = 10, # swarm size
    c.p = 0.5, # inherit best
    c.g = 0.5, # global best
    maxiter = 200, # iterations
    w0 = 1.2, # starting inertia weight
    wN = 0, # ending inertia weight
    save_traces = F, # save more information
    k = 50
  )
  control <- c(control, control_[!names(control_) %in% names(control)])

  # init data-structure
  X <- mrunif(
    nr = length(par), nc=control$s, lower=lower, upper=upper
  )
  if(all(!is.na(par))){
    X[, 1] <- par
  }
  X_fit <- apply(X, 2, fn)
  V <- mrunif(
    nr = length(par), nc=control$s,
    lower=-(upper-lower), upper=(upper-lower)
  )/4
  P <- X
  P_fit <- X_fit
  p_g <- P[, which.min(P_fit)]
  p_g_fit <- min(P_fit)

  neighbors <- sapply(1:control$s, function(x){ (-1+(x-round(control$k/2)-1):(x+round(control$k/2)-2)) %% control$s + 1 })

  trace_data <- NULL
  fit_data <- NULL
  for(i in 1:control$maxiter){

    P_g <- matrix(1, nrow=length(par), ncol=control$s)
    for(z in 1:ncol(P_g)){
      P_g[, z] <- P[, neighbors[which.min(P_fit[neighbors[, z]]), z]]
    }

    # move particles
    V <-
      (control$w0-(control$w0-control$wN)*i/control$maxiter) * V +
      control$c.p * runif(length(par)) * (P-X) +
      control$c.g * runif(length(par)) * (P_g-X)
    X <- X + V

    # set velocity to zeros if not in valid space
    V[X > upper] <- -0
    V[X < lower] <- -0

    # move into valid space
    X[X > upper] <- upper
    X[X < lower] <- lower

    # evaluate objective function
    X_fit <- apply(X, 2, fn)

    # save new previews best
    P[, P_fit > X_fit] <- X[, P_fit > X_fit]
    P_fit[P_fit > X_fit] <- X_fit[P_fit > X_fit]

    # # save new global best
    # if(any(P_fit < p_g_fit)){
    #   p_g <- P[, which.min(P_fit)]
    #   p_g_fit <- min(P_fit)
    # }

    if(control$save_traces){
      trace_data <- rbind(trace_data, data.frame("iter"=i, t(X)))
    }
    if(control$save_fit){
      fit_data <- rbind(fit_data, data.frame("iter"=i, "mean"=mean(P_fit), "best"=min(P_fit)))
    }
  }

  best <- which.min(P_fit)
  res <- list(
    "solution" = P[, best],
    "fitness" = P_fit[best]
  )
  if(control$save_traces){
    res$trace_data <- trace_data
  }
  if(control$save_fit){
    res$fit_data <- fit_data
  }
  return(res)
}






pso_res2 <- pso_default2(
  par = rep(0, ncol(pool_returns)),
  fn = function(x){
    fitness <- calc_fit(x)
    constraints <- calc_const(x)
    return(fitness+5*constraints)
  },
  lower = 0,
  upper = 0.05,
  control = list(
    s = 100, # swarm size
    c.p = 0.5, # inherit best
    c.g = 0.5, # global best
    maxiter = 50, # iterations
    w0 = 1.2, # starting inertia weight
    wN = 0, # ending inertia weight
    save_traces = F, # save more information
    save_fit = T,
    k=20
  )
)

plot_ly(data=pso_res2$fit_data, mode="lines", type = 'scatter') %>%
  add_trace(x=~iter, y=~mean, name="mean") %>%
  add_trace(x=~iter, y=~best, name="best")  %>%
  layout(yaxis=list(range=c(-0.1, 10)))




pso_res$fitness

pso_res2$fitness





