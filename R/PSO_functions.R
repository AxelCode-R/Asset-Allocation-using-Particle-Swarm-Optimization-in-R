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
      control$c.p * runif(length(par)) * (P-X) +
      control$c.g * runif(length(par)) * (p_g-X)
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
    P[, P_fit > X_fit] <- X[, P_fit > X_fit]
    P_fit[P_fit > X_fit] <- X_fit[P_fit > X_fit]

    # save new global best
    if(any(P_fit < p_g_fit)){
      p_g <- P[, which.min(P_fit)]
      p_g_fit <- min(P_fit)
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


  stagnate <- 0
  trace_fit <- NULL
  for(i in 1:control$maxiter){

    # move particles
    V <-
      (control$w0-(control$w0-control$wN)*i/control$maxiter) * V +
      control$c.p * runif(length(par)) * (P-X) +
      control$c.g * runif(length(par)) * (p_g-X)
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

    if(control$fn_stretching && stagnate>0.1*control$maxiter){
      fn1 <- function(pos){
        res <- fn(pos)
        G <- res + 10^4/2 * sqrt(sum((pos - p_g)^2)) * (sign(res - p_g_fit) + 1)
        H <- G + 0.5 * (sign(res - p_g_fit) + 1)/(tanh(10^-10 * (G - p_g_fit)))
        return(H)
      }
      stagnate <- 0
    }

    if(control$save_fit){
      trace_fit <- rbind(trace_fit, data.frame("iter"=i, "mean_fit" = mean(P_fit), "best_fit" = p_g_fit))
    }
  }

  res <- list(
    "solution" = p_g,
    "fitness" = p_g_fit
  )
  if(control$save_fit){
    res$trace_fit <- trace_fit
  }
  return(res)
}








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
  p_g <- P[, which.min(P_fit)]
  p_g_fit <- min(P_fit)

  neighbors <- sapply(1:control$s, function(x){ (-1+(x-round(control$k/2)-1):(x+round(control$k/2)-2)) %% control$s + 1 })

  trace_data <- NULL
  fit_data <- NULL
  for(i in 1:control$maxiter){

    # generate global best of each neighborhood
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
    V[X > upper] <- 0
    V[X < lower] <- 0

    # move into valid space
    X[X > upper] <- upper
    X[X < lower] <- lower

    # evaluate objective function
    X_fit <- apply(X, 2, fn)

    # save new previews best
    P[, P_fit > X_fit] <- X[, P_fit > X_fit]
    P_fit[P_fit > X_fit] <- X_fit[P_fit > X_fit]


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





# pso_preserving_feasibility <- function(
#     par,
#     fn_fit,
#     fn_const,
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
#     save_traces = F, # save more information
#     save_fit = F
#   )
#   control <- c(control, control_[!names(control_) %in% names(control)])
#
#   # init data-structure
#   X <- mrunif(
#     nr = length(par), nc=control$s, lower=lower, upper=upper
#   )
#   if(all(!is.na(par))){
#     X[, 1] <- par
#   }
#   X_fit <- apply(X, 2, fn)
#   V <- mrunif(
#     nr = length(par), nc=control$s,
#     lower=-(upper-lower), upper=(upper-lower)
#   )/10
#   P <- X
#   P_fit <- X_fit
#   p_g <- P[, which.min(P_fit)]
#   p_g_fit <- min(P_fit)
#
#
#   trace_data <- NULL
#   fit_data <- NULL
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
#     V[X > upper] <- 0#-V[X > upper]
#     V[X < lower] <- 0#-V[X < lower]
#
#     # move into valid space
#     X[X > upper] <- upper
#     X[X < lower] <- lower
#
#     # evaluate objective function
#     X_fit <- apply(X, 2, fn)
#
#     # save new previews best
#     P[, P_fit > X_fit] <- X[, P_fit > X_fit]
#     P_fit[P_fit > X_fit] <- X_fit[P_fit > X_fit]
#
#     # save new global best
#     if(any(P_fit < p_g_fit)){
#       p_g <- P[, which.min(P_fit)]
#       p_g_fit <- min(P_fit)
#     }
#
#     if(control$save_traces){
#       trace_data <- rbind(trace_data, data.frame("iter"=i, t(X)))
#     }
#     if(control$save_fit){
#       fit_data <- rbind(fit_data, data.frame("iter"=i, "mean"=mean(P_fit), "best"=p_g_fit))
#     }
#   }
#
#   res <- list(
#     "solution" = p_g,
#     "fitness" = p_g_fit
#   )
#   if(control$save_traces){
#     res$trace_data <- trace_data
#   }
#   if(control$save_fit){
#     res$fit_data <- fit_data
#   }
#   return(res)
# }





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
      control$c.p * runif(length(par)) * (P-X) +
      control$c.g * runif(length(par)) * (P_g-X)
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
