library(dplyr)
library(xts)
library(lubridate)

dates <- seq.Date(from = as.Date("2022-01-01"), to=as.Date("2022-01-20"), by="days")


fund <- NULL

pool_n <- 10


bm_pool <- xts(x = sapply(1:pool_n/1000, function(x){
  rnorm(length(dates), mean = x, sd = 0.01+0.005*runif(1)-x)
}), order.by = dates)

bm <- xts(x = bm_pool %*% (1:pool_n/sum(1:pool_n)), order.by = dates)

rule <- list()
rule$n <- 4

opt <- list()
opt$iter <- 10000



tracking_error <- function(fund_xts_return, bm_xts_return){
  sqrt(1/length(bm_xts_return) * sum((fund_xts_return - bm_xts_return)^2))
}

excess_return <- function(fund_xts_return, bm_xts_return){
  1/length(bm_xts_return) * sum(fund_xts_return - bm_xts_return)
}


fitness_f <- function(fund_xts_return, bm_xts_return, lambda){
  lambda * tracking_error(fund_xts_return, bm_xts_return) - (1-lambda) * excess_return(fund_xts_return, bm_xts_return)
}


init_funds <- matrix(runif(100), ncol = 10) %>%
  {./rowSums(.)}
row.names(init_funds) <- paste0("Fund_",1:10)



save <- NULL
for(i in 1:opt$iter){

  init_funds_return <- apply(t(init_funds), 2, function(x){
    bm_pool %*% x
  })

  fitness_funds <- apply(init_funds_return, 2, function(x){fitness_f(fund_xts_return=x, bm_xts_return=bm, lambda=0.5)})


  groups <- sample(c(rep(T,5),rep(F,5)), 10)
  parents <- names(c(which(fitness_funds==min(fitness_funds[groups]))[1],which(fitness_funds==min(fitness_funds[!groups]))[1]))
  cross_rand <- sample(c(rep(T,5),rep(F,5)), 10)
  child_1 <- colSums(init_funds[parents,] * matrix(c(cross_rand, !cross_rand), ncol=10, byrow = T))
  child_1 <- child_1 * (1+rnorm(10, mean = 0, sd = 0.01))
  #child_1[!child_1 %in% child_1[child_1 >= sort(child_1, decreasing = T)[rule$n]]] <- 0
  child_1[sample(1:length(child_1), size = sum(child_1!=0)-rule$n, prob = abs(child_1)/sum(abs(child_1)))] <- 0
  child_1 <- child_1/sum(child_1)
  child_2 <- colSums(init_funds[parents,] * matrix(c(!cross_rand, cross_rand), ncol=10, byrow = T))
  child_2 <- child_2 * (1+rnorm(10, mean = 0, sd = 0.01))
  #child_2[!child_2 %in% child_2[child_2 >= sort(child_2, decreasing = T)[rule$n]]] <- 0
  child_2[sample(1:length(child_2), size = sum(child_2!=0)-rule$n, prob = abs(child_2)/sum(abs(child_2)))] <- 0
  child_2 <- child_2/sum(child_2)

  child_1_fitness <- fitness_f(fund_xts_return = bm_pool %*% child_1, bm_xts_return = bm, lambda = 0.5)
  child_2_fitness <- fitness_f(fund_xts_return = bm_pool %*% child_2, bm_xts_return = bm, lambda = 0.5)
  if(child_1_fitness <= max(fitness_funds)*2){
    init_funds[names(fitness_funds[fitness_funds==max(fitness_funds)])[1],] <- child_1
  }
  if(child_2_fitness <= max(fitness_funds)*2){
    init_funds[names(fitness_funds[fitness_funds==max(fitness_funds)])[1],] <- child_2
  }

  save <- rbind(save, data.frame("i"=i, "all"=mean(fitness_funds), "child_1" = child_1_fitness, "child_2" = child_2_fitness))
}



init_funds_return <- apply(t(init_funds), 2, function(x){
  bm_pool %*% x
})

fitness_funds <- apply(init_funds_return, 2, function(x){fitness_f(fund_xts_return=x, bm_xts_return=bm, lambda=0.5)})

fitness_funds


library(plotly)

suppressPlotlyMessage <- function(p) {
  suppressMessages(plotly_build(p))
}

suppressPlotlyMessage({
  plot_ly(data = save, x=~i) %>%
    add_trace(y=~all, name="all", mode="lines") %>%
    add_trace(y=~child_1, name="child_1", mode="lines") %>%
    add_trace(y=~child_2, name="child_1", mode="lines") %>%
    layout(yaxis = list(range=list(0,max(0.0025,max(save[,-1])*1.05))))
})


child_1

