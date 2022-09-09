library(GA)
#remotes::install_github("DominikMueller64/BPSO")
source("global.R")
# library(BPSO)

from <- "2018-01-01"
to <- "2019-12-31"

spx_composition <- buffer(
  get_spx_composition(),
  "AS_spx_composition"
)


pool_returns_raw <- buffer(
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
pool_returns_raw <-
  pool_returns_raw[, colSums(is.na(pool_returns_raw))==0]


bm_returns <- buffer(
  get_yf(tickers = "%5EGSPC", from = from, to = to)$returns,
  "AS_sp500"
) %>% setNames(., "S&P 500")


# # aufgabe finde die 20 assets, die den S&P 500 am besten tracken
#
mat <- list(
  Dmat = cov(pool_returns_raw),
  dvec = cov(pool_returns_raw, bm_returns),
  Amat = t(rbind(
    rep(1, ncol(pool_returns_raw)), # sum up to 1
    diag(1,
         nrow=ncol(pool_returns_raw),
         ncol=ncol(pool_returns_raw)) # long only
  )),
  bvec = c(
    1, # sum up to 1
    rep(0, ncol(pool_returns_raw)) # long only
  ),
  meq = 1
)
# mat$Dmat_nPD <- as.matrix(nearPD(cov(mat$Dmat))$mat)
#
# res_bpso <- bpsoptim(
#   par = sample(c(rep(1, 30), rep(0, ncol(pool_returns_raw)-30))),
#   fn = function(x){
#     x <- as.logical(x)
#     if(sum(x)>30){
#       return(10+sum(x)-30)
#     }
#     qp <- solve.QP(
#       Dmat = if(is.positive.definite(mat$Dmat[x,x])){mat$Dmat[x,x]}else{mat$Dmat_nPD[x,x]}, dvec = mat$dvec[x],
#       Amat = mat$Amat[x, ], bvec = mat$bvec, meq = mat$meq
#     )
#     real_fit <- as.numeric(0.5 * t(qp$solution) %*% mat$Dmat[x,x] %*% qp$solution - t(mat$dvec[x]) %*% qp$solution)
#     return(real_fit)
#   },
#   control = list(
#     trace = T,
#     maxit = 200
#   )
# )
#
#
# fn = function(x){
#   x <- as.logical(x)
#   qp <- solve.QP(
#     Dmat = if(is.positive.definite(mat$Dmat[x,x])){mat$Dmat[x,x]}else{mat$Dmat_nPD[x,x]}, dvec = mat$dvec[x],
#     Amat = mat$Amat[x, ], bvec = mat$bvec, meq = mat$meq
#   )
#   real_fit <- as.numeric(0.5 * t(qp$solution) %*% mat$Dmat[x,x] %*% qp$solution - t(mat$dvec[x]) %*% qp$solution)
#   return(real_fit)
# }
#
#
# X <- apply(mrunif(nr = ncol(pool_returns_raw), nc = 100, lower = 0, upper = 1), 2, discretize_to)
# X_fit <- apply(X, 2, fn)
#
# P <- X
# P_fit <- X_fit
# p_g <- P[, which.min(P_fit)]
# p_g_fit <- which.min(P_fit)
#
#
#
#
#
# mat <- list(
#   Dmat = cov(pool_returns_raw),
#   dvec = cov(pool_returns_raw, bm_returns),
#   Amat = t(rbind(
#     rep(1, ncol(pool_returns_raw)), # sum up to 1
#     diag(1,
#          nrow=ncol(pool_returns_raw),
#          ncol=ncol(pool_returns_raw)) # long only
#   )),
#   bvec = c(
#     1, # sum up to 1
#     rep(0, ncol(pool_returns_raw)) # long only
#   ),
#   meq = 1
# )
#
# discretize_to <- function(x, n=30){
#   res <- rep(0, length(x))
#   res[order(x, decreasing = T)[1:n]] <- 1
#   return(res)
# }
#
# res_pso <- psoptim(
#   par = rep(NA, ncol(pool_returns_raw)),
#   fn = function(x){
#     x <- discretize_to(x, n=30)
#     qp <- solve.QP(
#       Dmat = if(is.positive.definite(mat$Dmat[x,x])){mat$Dmat[x,x]}else{as.matrix(nearPD(mat$Dmat[x,x])$mat)}, dvec = mat$dvec[x],
#       Amat = mat$Amat[x, ], bvec = mat$bvec, meq = mat$meq
#     )
#     real_fit <- as.numeric(0.5 * t(qp$solution) %*% mat$Dmat[x,x] %*% qp$solution - t(mat$dvec[x]) %*% qp$solution)
#     return(real_fit)
#   },
#   upper=1,
#   lower=0,
#   control = list(
#     trace = T,
#     maxit = 200
#   )
# )
#



#install.packages("GA")

N <- 5

discretize_to <- function(x, n){
  res <- rep(0, length(x))
  res[order(x, decreasing = T)[1:n]] <- 1
  return(res)
}


# VS naive
time_naive <- system.time({
  itp <- function(pool_returns, bm_returns){
    mat <- list(
      Dmat = cov(pool_returns),
      dvec = cov(pool_returns, bm_returns),
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

    qp <- solve.QP(
      Dmat = mat$Dmat, dvec = mat$dvec,
      Amat = mat$Amat, bvec = mat$bvec, meq = mat$meq
    )

    res <- list(
      "fit" = qp$value,
      "var" = as.numeric(
        var(pool_returns %*% qp$solution - bm_returns)),
      "solution" = setNames(qp$solution, colnames(pool_returns))
    )
  }

  res <- NULL
  step_size <- 1
  n_assets <- unique(c(seq(ncol(pool_returns_raw), N, -step_size), N))
  pool_returns_naive <- pool_returns_raw
  for(i in n_assets){

    temp <- if(i==max(n_assets)){
      itp(pool_returns_naive, bm_returns)
    }else{
      itp(
        pool_returns_naive[, names(sort(temp$solution, decreasing = T)[1:i])],
        bm_returns
      )
    }
    res <- rbind(
      res,
      data.frame("N"=i, "fit"=temp$fit, row.names = NULL)
    )
  }

  res_naive <- last(res)$fit
})



# GA approach
time_ga_fixed <- system.time({
  fn <- function(x){
    if(sum(x) > N || sum(x) <= 2){return(0)}
    x <- as.logical(x)
    qp <- solve.QP(
      Dmat = if(is.positive.definite(mat$Dmat[x,x])){mat$Dmat[x,x]}else{as.matrix(nearPD(mat$Dmat[x,x])$mat)}, dvec = mat$dvec[x],
      Amat = mat$Amat[x, ], bvec = mat$bvec, meq = mat$meq
    )
    real_fit <- as.numeric(0.5 * t(qp$solution) %*% mat$Dmat[x,x] %*% qp$solution - t(mat$dvec[x]) %*% qp$solution)
    return(-real_fit)
  }
  res_ga <- ga(
    type = "binary",
    fitness = fn,
    nBits = ncol(pool_returns_raw),
    crossover = gabin_uCrossover,
    popSize = 300,
    run=100,
    maxiter = 500,
    maxFitness = -last(res)$fit,
    suggestions=t(as.matrix(apply(mrunif(nr = ncol(pool_returns_raw), nc = 100, lower = 0, upper = 1), 2, discretize_to, n=N)))
  )


  x <- as.logical(res_ga@solution)
  qp <- solve.QP(
    Dmat = if(is.positive.definite(mat$Dmat[x,x])){mat$Dmat[x,x]}else{as.matrix(nearPD(mat$Dmat[x,x])$mat)}, dvec = mat$dvec[x],
    Amat = mat$Amat[x, ], bvec = mat$bvec, meq = mat$meq
  )
  res_ga_fixed <- qp$value

})




time_ga_long <- system.time({
  fn <- function(x){
    if(sum(x) > N || sum(x) <= 2){return(0)}
    x <- as.logical(x)
    qp <- solve.QP(
      Dmat = if(is.positive.definite(mat$Dmat[x,x])){mat$Dmat[x,x]}else{as.matrix(nearPD(mat$Dmat[x,x])$mat)}, dvec = mat$dvec[x],
      Amat = mat$Amat[x, ], bvec = mat$bvec, meq = mat$meq
    )
    real_fit <- as.numeric(0.5 * t(qp$solution) %*% mat$Dmat[x,x] %*% qp$solution - t(mat$dvec[x]) %*% qp$solution)
    return(-real_fit)
  }
  res_ga <- ga(
    type = "binary",
    fitness = fn,
    nBits = ncol(pool_returns_raw),
    crossover = gabin_uCrossover,
    popSize = 500,
    run=100,
    maxiter = 300,
    suggestions=t(as.matrix(apply(mrunif(nr = ncol(pool_returns_raw), nc = 100, lower = 0, upper = 1), 2, discretize_to, n=N)))
  )


  x <- as.logical(res_ga@solution)
  qp <- solve.QP(
    Dmat = if(is.positive.definite(mat$Dmat[x,x])){mat$Dmat[x,x]}else{as.matrix(nearPD(mat$Dmat[x,x])$mat)}, dvec = mat$dvec[x],
    Amat = mat$Amat[x, ], bvec = mat$bvec, meq = mat$meq
  )
  res_ga_long <- qp$value

})


data.frame(
  "type" = c("naive", "GA_fixed", "GA_long"),
  "fit" = c(res_naive, res_ga_fixed, res_ga_long),
  "time" = c(time_naive[3], time_ga_fixed[3], time_ga_long[3])
)
