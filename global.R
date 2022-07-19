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

# alphavantage API-KEY
api_key <- Sys.getenv("api_key")

for(file in list.files("R/", full.names=T)){
  source(file)
}



