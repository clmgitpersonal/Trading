require(quantstrat)
#Load ETFs from yahoo
currency("USD")
symbols = c("AWK","CHSP","HIW","HPT","MCD","VRSN","V","WFC")
stock(symbols, currency="USD",multiplier=1)
getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from='2014-02-28', to='2014-02-28')