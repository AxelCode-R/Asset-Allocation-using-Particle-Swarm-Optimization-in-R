# Wrapper for all PSOs
pso <- function(type="default", ...){
  if(type=="default"){
    return(pso_default(...))
  }
}



# Standard PSO
pso_default <- function(
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
    save_fit = F
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
  )/10
  P <- X
  P_fit <- X_fit
  p_g <- P[, which.min(P_fit)]
  p_g_fit <- min(P_fit)


  trace_data <- NULL
  fit_data <- NULL
  for(i in 1:control$maxiter){

    # move particles
    V <-
      (control$w0-(control$w0-control$wN)*i/control$maxiter) * V +
      control$c.p * t(runif(ncol(X)) * t(P-X)) +
      control$c.g * t(runif(ncol(X)) * t(p_g-X))
    X <- X + V

    # set velocity to zeros if not in valid space
    V[X > upper] <- 0#-V[X > upper]
    V[X < lower] <- 0#-V[X < lower]

    # move into valid space
    X[X > upper] <- upper
    X[X < lower] <- lower

    # evaluate objective function
    X_fit <- apply(X, 2, fn)

    # save new previews best
    P[, P_fit >= X_fit] <- X[, P_fit >= X_fit]
    P_fit[P_fit >= X_fit] <- X_fit[P_fit >= X_fit]

    # save new global best
    if(any(P_fit <= p_g_fit)){
      sel <- sample(which(P_fit==min(P_fit)), 1)
      p_g <- P[, sel]
      p_g_fit <- P_fit[sel]
    }

    if(control$save_traces){
      trace_data <- rbind(trace_data, data.frame("iter"=i, t(X)))
    }
    if(control$save_fit){
      fit_data <- rbind(fit_data, data.frame("iter"=i, "mean"=mean(P_fit), "best"=p_g_fit))
    }
  }

  res <- list(
    "solution" = p_g,
    "fitness" = p_g_fit
  )
  if(control$save_traces){
    res$trace_data <- trace_data
  }
  if(control$save_fit){
    res$fit_data <- fit_data
  }
  return(res)
}






pso_fn_stretching <- function(
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
    save_fit = F, # save more information
    fn_stretching = F
  )
  control <- c(control, control_[!names(control_) %in% names(control)])

  fn1 <- function(pos){fn(pos)}

  # init data-structure
  X <- mrunif(
    nr = length(par), nc=control$s, lower=lower, upper=upper
  )
  if(all(!is.na(par))){
    X[, 1] <- par
  }
  X_fit <- apply(X, 2, fn1)
  V <- mrunif(
    nr = length(par), nc=control$s,
    lower=-(upper-lower), upper=(upper-lower)
  )/10
  P <- X
  P_fit <- X_fit
  p_g <- P[, which.min(P_fit)]
  p_g_fit <- min(P_fit)
  # save_p_g <- p_g
  # save_p_g_fit <- p_g_fit

  stagnate <- 0
  trace_fit <- NULL
  for(i in 1:control$maxiter){

    # move particles
    V <-
      (control$w0-(control$w0-control$wN)*i/control$maxiter) * V +
      control$c.p * t(runif(ncol(X)) * t(P-X)) +
      control$c.g * t(runif(ncol(X)) * t(p_g-X))
    X <- X + V

    # set velocity to zeros if not in valid space
    V[X > upper] <- 0
    V[X < lower] <- 0

    # move into valid space
    X[X > upper] <- upper
    X[X < lower] <- lower

    # evaluate objective function
    X_fit <- apply(X, 2, fn1)

    # save new previews best
    P[, P_fit > X_fit] <- X[, P_fit > X_fit]
    P_fit[P_fit > X_fit] <- X_fit[P_fit > X_fit]

    # save new global best
    if(any(P_fit < p_g_fit)){
      p_g <- P[, which.min(P_fit)]
      p_g_fit <- min(P_fit)
    }else{
      stagnate <- stagnate + 1
    }

    if(control$fn_stretching && stagnate>10 && i < (control$maxiter-20)){

      fn1 <- function(pos){
        res <- fn(pos)
        G <- res + 10^4/2 * sqrt(sum((pos - fn1_p_g)^2)) * (sign(res - fn1_p_g_fit) + 1)
        H <- G + 0.5 * (sign(res - fn1_p_g_fit) + 1)/(tanh(10^(-10) * (G - fn1_p_g_fit)))
        return(H)
      }
      fn1_p_g <- p_g
      fn1_p_g_fit <- p_g_fit

      P_fit <- apply(P, 2, fn1)

      stagnate <- 0


      # X <- mrunif(
      #   nr = length(par), nc=control$s, lower=lower, upper=upper
      # )
      # V <- mrunif(
      #   nr = length(par), nc=control$s,
      #   lower=-(upper-lower), upper=(upper-lower)
      # )/10


      # P_fit <- apply(P, 2, fn1)
      #
      # save_p_g <- p_g
      # save_p_g_fit <- p_g_fit
      # p_g <- P[, which.min(P_fit)]
      # p_g_fit <- min(P_fit)

    }

    if(control$save_fit){
      trace_fit <- rbind(trace_fit, data.frame("iter"=i, "mean_fit" = mean(P_fit), "best_fit" = p_g_fit ))
    }
  }

  # if(save_p_g_fit < p_g_fit){
  #   p_g_fit <- save_p_g_fit
  #   p_g <- save_p_g
  # }

  res <- list(
    "solution" = p_g,
    "fitness" = p_g_fit
  )
  if(control$save_fit){
    res$trace_fit <- trace_fit
  }
  return(res)
}

# pso_fn_stretching2 <- function(
#     par,
#     fn,
#     lower,
#     upper,
#     control = list()
# ){
#
#   # use default control values if not set
#   control_ = list(
#     s = 10, # swarm size
#     c.p = 0.5, # inherit best
#     c.g = 0.5, # global best
#     maxiter = 200, # iterations
#     w0 = 1.2, # starting inertia weight
#     wN = 0, # ending inertia weight
#     save_fit = F, # save more information
#     fn_stretching = F
#   )
#   control <- c(control, control_[!names(control_) %in% names(control)])
#
#   fn1 <- function(pos){fn(pos)}
#
#   # init data-structure
#   X <- mrunif(
#     nr = length(par), nc=control$s, lower=lower, upper=upper
#   )
#   if(all(!is.na(par))){
#     X[, 1] <- par
#   }
#   X_fit <- apply(X, 2, fn1)
#   V <- mrunif(
#     nr = length(par), nc=control$s,
#     lower=-(upper-lower), upper=(upper-lower)
#   )/10
#   P <- X
#   P_fit <- X_fit
#   p_g <- P[, which.min(P_fit)]
#   p_g_fit <- min(P_fit)
#   save_p_g <- p_g
#   save_p_g_fit <- p_g_fit
#
#   stagnate <- 0
#   trace_fit <- NULL
#   for(i in 1:control$maxiter){
#
#     # move particles
#     V <-
#       (control$w0-(control$w0-control$wN)*i/control$maxiter) * V +
#       control$c.p * runif(length(par)) * (P-X) +
#       control$c.g * runif(length(par)) * (p_g-X)
#     X <- X + V
#
#     # set velocity to zeros if not in valid space
#     V[X > upper] <- 0
#     V[X < lower] <- 0
#
#     # move into valid space
#     X[X > upper] <- upper
#     X[X < lower] <- lower
#
#     # evaluate objective function
#     X_fit <- apply(X, 2, fn1)
#
#     # save new previews best
#     P[, P_fit > X_fit] <- X[, P_fit > X_fit]
#     P_fit[P_fit > X_fit] <- X_fit[P_fit > X_fit]
#
#     # save new global best
#     if(any(P_fit < p_g_fit)){
#       p_g <- P[, which.min(P_fit)]
#       p_g_fit <- min(P_fit)
#     }else{
#       stagnate <- stagnate + 1
#     }
#
#     if(control$fn_stretching && stagnate>(0.20*control$maxiter) && i < (0.85*control$maxiter)){
#       #break()
#       fn1 <- function(pos){
#         res <- fn(pos)
#         G <- res + 10^4/2 * sqrt(sum((pos - fn1_p_g)^2))/length(pos) * (sign(res - fn1_p_g_fit) + 1)
#         H <- G + 0.5 * (sign(res - fn1_p_g_fit) + 1)/(tanh(10^(-10) * (G - fn1_p_g_fit)))
#         return(H)
#       }
#       fn1_p_g <- p_g
#       fn1_p_g_fit <- p_g_fit
#
#       #P_fit <- apply(P, 2, fn1)
#
#       stagnate <- 0
#
#
#       # X <- mrunif(
#       #   nr = length(par), nc=control$s, lower=lower, upper=upper
#       # )
#       # V <- mrunif(
#       #   nr = length(par), nc=control$s,
#       #   lower=-(upper-lower), upper=(upper-lower)
#       # )/10
#
#
#       P_fit <- apply(P, 2, fn1)
#
#       save_p_g <- p_g
#       save_p_g_fit <- p_g_fit
#       p_g <- P[, which.min(P_fit)]
#       p_g_fit <- min(P_fit)
#
#     }
#
#     if(control$save_fit){
#       trace_fit <- rbind(trace_fit, data.frame("iter"=i, "mean_fit" = mean(P_fit), "best_fit" = min(p_g_fit, save_p_g_fit) ))
#     }
#   }
#
#   if(save_p_g_fit < p_g_fit){
#     p_g_fit <- save_p_g_fit
#     p_g <- save_p_g
#   }
#
#   res <- list(
#     "solution" = p_g,
#     "fitness" = p_g_fit
#   )
#   if(control$save_fit){
#     res$trace_fit <- trace_fit
#   }
#   return(res)
# }







pso_local <- function(
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
    save_fit = F,
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
  )/10
  P <- X
  P_fit <- X_fit

  neighbors <- sapply(1:control$s, function(x){ (-1+(x-round(control$k/2)-1):(x+round(control$k/2)-2)) %% control$s + 1 })

  trace_data <- NULL
  fit_data <- NULL
  for(i in 1:control$maxiter){

    # generate global best of each neighborhood
    P_g <- matrix(1, nrow=length(par), ncol=control$s)
    for(z in 1:ncol(P_g)){
      P_g[, z] <- P[, neighbors[sample(which(P_fit[neighbors[, z]]==min(P_fit[neighbors[, z]])), 1), z]]
    }

    # move particles
    V <-
      (control$w0-(control$w0-control$wN)*i/control$maxiter) * V +
      control$c.p * t(runif(ncol(X)) * t(P-X)) +
      control$c.g * t(runif(ncol(X)) * t(P_g-X))
    X <- X + V

    # set velocity to zeros if not in valid space
    V[X > upper] <- 0
    V[X < lower] <- 0

    # move into valid space
    X[X > upper] <- upper
    X[X < lower] <- lower

    # evaluate objective function
    X_fit <- apply(X, 2, fn)

    # save new previews best
    P[, P_fit >= X_fit] <- X[, P_fit >= X_fit]
    P_fit[P_fit >= X_fit] <- X_fit[P_fit >= X_fit]


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





pso_self_adaptive_velocity <- function(
    par,
    fn,
    lower,
    upper,
    control = list()
){

  # use default control values if not set
  control_ = list(
    s = 10, # swarm size
    maxiter = 200, # iterations
    save_traces = F, # save more information
    save_fit = F,
    Sp = 0.8, # selection probability of velocity strategy
    Cp = 0.7 # selection probability of boundary operations
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
  )/10
  P <- X
  P_fit <- X_fit
  p_g <- P[, which.min(P_fit)]
  p_g_fit <- min(P_fit)

  ac_params <- data.frame("w"=rep(0.5, control$s), "c.p"=rep(2, control$s), "c.g"=rep(2, control$s))

  trace_data <- NULL
  fit_data <- NULL
  for(i in 1:control$maxiter){

    mu1 <- 0.1*(1-(i/control$maxiter)^2)+0.3
    sig1 <- 0.1
    mu2 <- 0.4*(1-(i/control$maxiter)^2)+0.2
    sig2 <- 0.4

    for(p in 1:control$s){
      if(runif(1) > control$Sp){
        V[,p] <- ac_params[p,]$w * V[,p] +
          ac_params[p,]$c.p * runif(1) * (P[,p]-X[,p]) +
          ac_params[p,]$c.g * runif(1) * (p_g-X[,p])
      }else{
        if(runif(1) < 0.5){
          V[,p] <- ac_params[p,]$w * V[,p] +
            ac_params[p,]$c.p * rcauchy(1, mu1, sig1) * (P[,p]-X[,p]) +
            ac_params[p,]$c.g * rcauchy(1, mu1, sig1) * (p_g-X[,p])
        }else{
          V[,p] <- ac_params[p,]$w * V[,p] +
            ac_params[p,]$c.p * rcauchy(1, mu2, sig2) * (P[,p]-X[,p]) +
            ac_params[p,]$c.g * rcauchy(1, mu2, sig2) * (p_g-X[,p])
        }
      }
    }
    X <- X + V

    upper_breaks <- X > upper
    ub_ind <- which(upper_breaks==T, arr.ind = T)
    if(nrow(ub_ind)>0){
      for(k in 1:nrow(ub_ind)){
        if(runif(1) > control$Cp){
          X[ub_ind[k,1],ub_ind[k,2]] <- runif(1, lower, upper)
        }else{
          X[ub_ind[k,1],ub_ind[k,2]] <- upper
        }
      }
    }

    lower_breaks <- X < lower
    lb_ind <- which(lower_breaks==T, arr.ind = T)
    if(nrow(lb_ind)>0){
      for(k in 1:nrow(lb_ind)){
        if(runif(1) > control$Cp){
          X[lb_ind[k,1],lb_ind[k,2]] <- runif(1, lower, upper)
        }else{
          X[lb_ind[k,1],lb_ind[k,2]] <- lower
        }
      }
    }




    # # set velocity to zeros if not in valid space
    # V[X > upper] <- 0#-V[X > upper]
    # V[X < lower] <- 0#-V[X < lower]
    #
    # # move into valid space
    # X[X > upper] <- upper
    # X[X < lower] <- lower

    # evaluate objective function
    X_fit <- apply(X, 2, fn)

    max_fit <- max(X_fit)
    WG <- abs(X_fit-max_fit)/sum(abs(X_fit-max_fit))
    ac_params$w <- rcauchy(control$s, sum(WG*ac_params$w), 0.2)
    ac_params$c.p <- rcauchy(control$s, sum(WG*ac_params$c.p), 0.3)
    ac_params$c.g <- rcauchy(control$s, sum(WG*ac_params$c.g), 0.3)

    ac_params$w[ac_params$w > 1] <- runif(1)
    ac_params$w[ac_params$w < 0] <- runif(1)/10

    ac_params$c.p[ac_params$c.p > 4] <- runif(1)*4
    ac_params$c.p[ac_params$c.p < 0] <- runif(1)

    ac_params$c.g[ac_params$c.g > 4] <- runif(1)*4
    ac_params$c.g[ac_params$c.g < 0] <- runif(1)


    # save new previews best
    P[, P_fit >= X_fit] <- X[, P_fit >= X_fit]
    P_fit[P_fit >= X_fit] <- X_fit[P_fit >= X_fit]

    # save new global best
    if(any(P_fit < p_g_fit)){
      sel <- sample(which(P_fit==min(P_fit)), 1)
      p_g <- P[, sel]
      p_g_fit <- P_fit[sel]
    }

    if(control$save_traces){
      trace_data <- rbind(trace_data, data.frame("iter"=i, t(X)))
    }
    if(control$save_fit){
      fit_data <- rbind(fit_data, data.frame("iter"=i, "mean"=mean(P_fit), "best"=p_g_fit))
    }
  }

  res <- list(
    "solution" = p_g,
    "fitness" = p_g_fit
  )
  if(control$save_traces){
    res$trace_data <- trace_data
  }
  if(control$save_fit){
    res$fit_data <- fit_data
  }
  return(res)
}






pso_preserving_feasibility <- function(
    par,
    fn_fit,
    fn_const,
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
    save_fit = F
  )
  control <- c(control, control_[!names(control_) %in% names(control)])

  # init data-structure
  X <- mrunif(
    nr = length(par), nc=control$s, lower=lower, upper=upper
  )
  if(all(!is.na(par))){
    X[, 1] <- par
  }
  X_fit <- apply(X, 2, fn_fit)
  X_const <- apply(X, 2, fn_const)

  V <- mrunif(
    nr = length(par), nc=control$s,
    lower=-(upper-lower), upper=(upper-lower)
  )/10
  P <- X
  P_fit <- X_fit
  P_const <- X_const
  p_g <- P[, which.min(P_fit + 10^6*P_const!=0)]
  p_g_fit <- P_fit[which.min(P_fit + 10^6*P_const!=0)]


  trace_data <- NULL
  fit_data <- NULL
  for(i in 1:control$maxiter){

    # move particles
    V <-
      (control$w0-(control$w0-control$wN)*i/control$maxiter) * V +
      control$c.p * t(runif(ncol(X)) * t(P-X)) +
      control$c.g * t(runif(ncol(X)) * t(p_g-X))
    X <- X + V

    # set velocity to zeros if not in valid space
    V[X > upper] <- 0#-V[X > upper]
    V[X < lower] <- 0#-V[X < lower]

    # move into valid space
    X[X > upper] <- upper
    X[X < lower] <- lower

    # evaluate objective function
    X_fit <- apply(X, 2, fn_fit)
    X_const <- apply(X, 2, fn_const)

    # save new previews best
    P[, P_fit > X_fit & X_const == 0] <- X[, P_fit > X_fit & X_const == 0]
    P_const[P_fit > X_fit & X_const == 0] <- X_const[P_fit > X_fit & X_const == 0]
    P_fit[P_fit > X_fit & X_const == 0] <- X_fit[P_fit > X_fit & X_const == 0]

    # save new global best
    if(any(P_fit < p_g_fit & P_const == 0)){
      p_g <- P[, which.min(P_fit + 10^6*P_const!=0)]
      p_g_fit <- P_fit[which.min(P_fit + 10^6*P_const!=0)]
    }

    if(control$save_traces){
      trace_data <- rbind(trace_data, data.frame("iter"=i, t(X)))
    }
    if(control$save_fit){
      fit_data <- rbind(fit_data, data.frame("iter"=i, "mean"=mean(P_fit), "best"=p_g_fit))
    }
  }

  res <- list(
    "solution" = p_g,
    "fitness" = p_g_fit
  )
  if(control$save_traces){
    res$trace_data <- trace_data
  }
  if(control$save_fit){
    res$fit_data <- fit_data
  }
  return(res)
}





pso_local_prevFeas <- function(
    par,
    fn_fit,
    fn_const,
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
    save_fit = F,
    k = 50
  )
  control <- c(control, control_[!names(control_) %in% names(control)])


  X <- mrunif(
    nr = length(par), nc=control$s, lower=lower, upper=upper
  )
  if(all(!is.na(par))){
    X[, 1] <- par
  }


  X_fit <- apply(X, 2, fn_fit)
  X_const <- apply(X, 2, fn_const)

  V <- mrunif(
    nr = length(par), nc=control$s,
    lower=-(upper-lower), upper=(upper-lower)
  )/10
  P <- X
  P_fit <- X_fit
  P_const <- X_const
  p_g <- P[, which.min(P_fit)]
  p_g_fit <- min(P_fit)
  p_g_const <- min(P_const)

  neighbors <- sapply(1:control$s, function(x){ (-1+(x-round(control$k/2)-1):(x+round(control$k/2)-2)) %% control$s + 1 })

  trace_data <- NULL
  fit_data <- NULL
  for(i in 1:control$maxiter){

    # generate global best of each neighborhood
    P_g <- matrix(NA, nrow=length(par), ncol=control$s)
    for(z in 1:ncol(P_g)){
      neigh <- neighbors[, z]
      neigh_noConst <- neigh[P_const[neigh]==0]
      if(length(neigh_noConst)==0){
        P_g[, z] <- p_g
      }else{
        P_g[, z] <- P[, neighbors[which.min(P_fit[neigh_noConst]), z]]
      }
    }

    # if(all(P_g==p_g)){
    #   print("gb-noCo: no")
    # }else{
    #   print("gb-noCo: yes")
    # }

    # move particles
    V <-
      (control$w0-(control$w0-control$wN)*i/control$maxiter) * V +
      control$c.p * t(runif(ncol(X)) * t(P-X)) +
      control$c.g * t(runif(ncol(X)) * t(p_g-X))
    X <- X + V

    # set velocity to zeros if not in valid space
    V[X > upper] <- 0
    V[X < lower] <- 0

    # move into valid space
    X[X > upper] <- upper
    X[X < lower] <- lower

    # evaluate objective function
    X_fit <- apply(X, 2, fn_fit)
    X_const <- apply(X, 2, fn_const)

    # save new previews best
    sel <- (P_fit+P_const) > (X_fit+X_const)
    P[, sel] <- X[, sel]
    P_fit[sel] <- X_fit[sel]
    P_const[sel] <- X_const[sel]

    # save new global best
    if( (p_g_const != 0 && any(P_const == 0)) || ((p_g_const == 0) && any( (P_const == 0) & (P_fit < p_g_fit))) ){
      sel <- which(P_fit == min(P_fit[which(P_const==0)]))[1]
      p_g <- P[, sel]
      p_g_fit <- P_fit[sel]
      p_g_const <- P_const[sel]
      #print("pg-noC: changed!!!!!")
    }


    if(control$save_traces){
      trace_data <- rbind(trace_data, data.frame("iter"=i, t(X)))
    }
    if(control$save_fit){
      fit_data <- rbind(fit_data, data.frame("iter"=i, "mean"=mean(P_fit), "best"=min(P_fit)))
    }
  }

  best <- which.min(P_fit)
  res <- list(
    "solution" = p_g,
    "fitness" = p_g_fit,
    "const" = p_g_const
  )
  if(control$save_traces){
    res$trace_data <- trace_data
  }
  if(control$save_fit){
    res$fit_data <- fit_data
  }
  return(res)
}
