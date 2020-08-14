library(quantmod)
library(TTR)
url <- "https://finance.yahoo.com/quote/MSFT/history?p=MSFT"
data <- read.csv(url)
startDate = as.Date("2016-06-01")
endDate = as.Date("2017-07-31")

getSymbols("MSFT", from = startDate, to = endDate)
allSym <- stockSymbols()
prices <- data$cl
log_returns <- diff(log(prices), lag=1)