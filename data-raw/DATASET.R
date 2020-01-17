## code to prepare `DATASET` dataset goes here
library(quantmod)
from <- "USD"
to <- "MXN"
B <- getQuote(paste0(from,to,"=X"))

# DATA LTC

LTC_DAILY <- read.csv("data-raw/LTC-USD_daily.csv")
LTC_DAILY$Close_MXN <- LTC_DAILY$Adj.Close * B$Last
usethis::use_data(LTC_DAILY)


LTC_WEEKLY <- read.csv("data-raw/LTC-USD_weekly.csv")
LTC_WEEKLY$Close_MXN <- LTC_WEEKLY$Adj.Close * B$Last
usethis::use_data(LTC_WEEKLY)

LTC_MONTHLY <- read.csv("data-raw/LTC-USD_monthly.csv")
LTC_MONTHLY$Close_MXN <- LTC_MONTHLY$Adj.Close * B$Last
usethis::use_data(LTC_MONTHLY)

# DATA ETH

ETH_DAILY <- read.csv("data-raw/ETH-USD_daily.csv")
ETH_DAILY$Close_MXN <- ETH_DAILY$Adj.Close * B$Last
usethis::use_data(ETH_DAILY)

ETH_WEEKLY <- read.csv("data-raw/ETH-USD_weekly.csv")
ETH_WEEKLY$Close_MXN <- ETH_WEEKLY$Adj.Close * B$Last
usethis::use_data(ETH_WEEKLY)

ETH_MONTHLY <- read.csv("data-raw/ETH-USD_monthly.csv")
ETH_MONTHLY$Close_MXN <- ETH_MONTHLY$Adj.Close * B$Last
usethis::use_data(ETH_MONTHLY)

# DATA BAT

BAT_DAILY <- read.csv("data-raw/BAT-USD_daily.csv")
BAT_DAILY$Close_MXN <- BAT_DAILY$Adj.Close * B$Last
usethis::use_data(BAT_DAILY)

BAT_WEEKLY <- read.csv("data-raw/BAT-USD_weekly.csv")
BAT_WEEKLY$Close_MXN <- BAT_WEEKLY$Adj.Close * B$Last
usethis::use_data(BAT_WEEKLY)

BAT_MONTHLY <- read.csv("data-raw/BAT-USD_monthly.csv")
BAT_MONTHLY$Close_MXN <- BAT_MONTHLY$Adj.Close * B$Last
usethis::use_data(BAT_MONTHLY)

# DATA BCH

BCH_DAILY <- read.csv("data-raw/BCH-USD_daily.csv")
BCH_DAILY$Close_MXN <- BCH_DAILY$Adj.Close * B$Last
usethis::use_data(BCH_DAILY)

BCH_WEEKLY <- read.csv("data-raw/BCH-USD_weekly.csv")
BCH_WEEKLY$Close_MXN <- BCH_WEEKLY$Adj.Close * B$Last
usethis::use_data(BCH_WEEKLY)

BCH_MONTHLY <- read.csv("data-raw/BCH-USD_monthly.csv")
BCH_MONTHLY$Close_MXN <- BCH_MONTHLY$Adj.Close * B$Last
usethis::use_data(BCH_MONTHLY)

# DATA BTC

BTC_DAILY <- read.csv("data-raw/BTC-USD_daily.csv")
BTC_DAILY$Close_MXN <- BTC_DAILY$Adj.Close * B$Last
usethis::use_data(BTC_DAILY)

BTC_WEEKLY <- read.csv("data-raw/BTC-USD_weekly.csv")
BTC_WEEKLY$Close_MXN <- BTC_WEEKLY$Adj.Close * B$Last
usethis::use_data(BTC_WEEKLY)

BTC_MONTHLY <- read.csv("data-raw/BTC-USD_monthly.csv")
BTC_MONTHLY$Close_MXN <- BTC_MONTHLY$Adj.Close * B$Last
usethis::use_data(BTC_MONTHLY)

# DATA XRP

XRP_DAILY <- read.csv("data-raw/XRP-USD_daily.csv")
XRP_DAILY$Close_MXN <- XRP_DAILY$Adj.Close * B$Last
usethis::use_data(XRP_DAILY)

XRP_WEEKLY <- read.csv("data-raw/XRP-USD_weekly.csv")
XRP_WEEKLY$Close_MXN <- XRP_WEEKLY$Adj.Close * B$Last
usethis::use_data(XRP_WEEKLY)

XRP_MONTHLY <- read.csv("data-raw/XRP-USD_monthly.csv")
XRP_MONTHLY$Close_MXN <- XRP_MONTHLY$Adj.Close * B$Last
usethis::use_data(XRP_MONTHLY)
