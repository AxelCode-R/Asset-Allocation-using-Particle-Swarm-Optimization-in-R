options(scipen=999)
options(stringsAsFactors = FALSE)
options(max.print=100)
library(lubridate)
library(alphavantager)
library(dplyr)
library(readr)
library(tidyverse)
library(rvest)
library(xts)
library(quadprog)
library(Matrix)
library(matrixcalc)
library(plotly)
library(pso)
library(data.table)
library(corrr)
library(webshot2)
#library(knitr)
# install.packages("webshot2")
# install.packages("webshot")
# webshot::install_phantomjs()


# alphavantage API-KEY
api_key <- Sys.getenv("api_key")

for(file in list.files("R/", full.names=T)){
  source(file)
}



