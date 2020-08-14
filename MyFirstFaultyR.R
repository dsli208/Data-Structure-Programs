library(quantmod)
library(plyr)

symbols <- c("MSFT", "AMZN") #Defining the ticker names

l_ply(symbols, function(sym) try(getSymbols(sym)))

symbols <- symbols[symbols %in% ls()]

sym.list <- llply(symbols, get)

data <- xts()
for (i in seq_along(symbols)) {
  symbol <- symbols[[i]]
  data <- merge(data, get(symbol)[, paste(symbol, "Close", sep = ". ")])
}
