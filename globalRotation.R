require(quantmod)
require(PerformanceAnalytics)
# take five markets (e.g. the global markets MDY, ILF, FEZ, EEM, and EPP), 
# along with a treasury ETF (TLT), and every month, fully invest
# in the security that had the best momentum.
globalrotation <- c("MDY",          # Mid Cap 400
                    "TLT",          # 20 yr US Treas
                    "EEM",
                    "ILF",          #Latin America
                    "EPP",          #MSCI Pacific ex Japan
                    "FEZ"           #DJ Euro Stoxx 50
                    )
usnewsbestmutualfunds <- c("SPY",
                          #"PRFHX","MMHAX","AMHIX", #Hi Yld
                           "VWAHX","SFBDX","VWITX", #Natl Inter
                           "VWLTX","FHIGX","PRINX", #Natl Long
                           "AITFX","GTCMX","MUISX", #Natl Short
                           "USVAX", # VIrginia
                           "SHY"     # Short Taxable
                           )
coffeehouse <- c("VFINX","VIVAX","NAESX","VISVX","VGSIX","VGTSX","VBMFX","SHY")
wellmuni <- c("VWELX","VWINX","MUB")
permanentrotation <- c("GLD","TLT","SHY","SPY")
pictet <- c("SPY","GLD","EEM","IEF")
symbols <- usnewsbestmutualfunds

symbols <- coffeehouse
symbols <- wellmuni
symbols <- permanentrotation
symbols <- globalrotation
symbols <- pictet

getSymbols(symbols, from="2000-01-01")
prices <- list()
for(i in 1:length(symbols)) {
  prices[[i]] <- Ad(get(symbols[i]))
}
prices <- do.call(cbind, prices)
colnames(prices) <- gsub("\\.[A-z]*", "", colnames(prices))
returns <- Return.calculate(prices)
returns <- na.omit(returns)

logicInvestGMR <- function(returns, lookback = 3) {
  ep <- endpoints(returns, on = "months") 
  weights <- list()
  for(i in 2:(length(ep) - lookback)) {
    retSubset <- returns[ep[i]:ep[i+lookback],]
    cumRets <- Return.cumulative(retSubset)
    rankCum <- rank(cumRets)
    weight <- rep(0, ncol(retSubset))
    weight[which.max(cumRets)] <- 1
    weight <- xts(t(weight), order.by=index(last(retSubset)))
    weights[[i]] <- weight
  }
  weights <- do.call(rbind, weights)
  stratRets <- Return.portfolio(R = returns, weights = weights)
  return(stratRets)
}

gmr <- logicInvestGMR(returns)
charts.PerformanceSummary(gmr)
rbind(table.AnnualizedReturns(gmr), maxDrawdown(gmr), CalmarRatio(gmr))