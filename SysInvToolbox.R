# Load Systematic Investor Toolbox (SIT): from file, if for example you saved sit.gz to  /Users/Chuck/Dropbox/R/Trading/sit.gz 
############################################################################### 
con = gzcon(file('/Users/Chuck/Dropbox/R/Trading/sit.gz', 'rb'))
source(con)
close(con)


#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')

tickers = spl('SPY,^VIX')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', to = Sys.Date(),
           env = data, auto.assign = T)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', fill.gaps = T)

VIX = Cl(data$VIX)
bt.prep.remove.symbols(data, 'VIX')


#*****************************************************************
# Setup
#*****************************************************************
prices = data$prices

models = list()

#*****************************************************************
# 200 SMA
#****************************************************************** 
data$weight[] = NA
data$weight[] = iif(prices > SMA(prices, 200), 1, 0)
models$ma200 = bt.run.share(data, clean.signal=T)

#*****************************************************************
# 200 ROC
#****************************************************************** 
roc = prices / mlag(prices) - 1

data$weight[] = NA
data$weight[] = iif(SMA(roc, 200) > 0, 1, 0)
models$roc200 = bt.run.share(data, clean.signal=T)

#*****************************************************************
# 200 VIX MOM
#****************************************************************** 
data$weight[] = NA
data$weight[] = iif(SMA(roc/VIX, 200) > 0, 1, 0)
models$vix.mom = bt.run.share(data, clean.signal=T)

#*****************************************************************
# 200 ER MOM
#****************************************************************** 
forecast = SMA(roc,10)
error = roc - mlag(forecast)
mae = SMA(abs(error), 10)

data$weight[] = NA
data$weight[] = iif(SMA(roc/mae, 200) > 0, 1, 0)
models$er.mom = bt.run.share(data, clean.signal=T)

#*****************************************************************
# Report
#****************************************************************** 
strategy.performance.snapshoot(models, T)










############################################################################## 
# Select top N for each period
############################################################################### 
ntop <- function
(
  data,		# matrix with observations
  topn = 1, 	# top n
  dirMaxMin = TRUE
) 
{
  out = data
  out[] = NA
  
  for( i in 1:nrow(data) ) {
    x = coredata(data[i,])
    o = sort.list(x, na.last = TRUE, decreasing = dirMaxMin)
    index = which(!is.na(x))
    x[] = NA
    
    if(len(index)>0) {
      n = min(topn, len(index))
      x[o[1:n]] = 1/n
    }
    out[i,] = x
  }
  out[is.na(out)] = 0	
  return( out )
  
  
  
  
  ############################################################################## 
  # Select top N for each period, and keep them till they drop below keepn rank
  ############################################################################### 
  ntop.keep <- function
  (
    data, 		# matrix with observations
    topn = 1, 	# top n
    keepn = 1, 	# keep n
    dirMaxMin = TRUE
  ) 
  {
    out = data
    out[] = NA
    
    for( i in 1:nrow(data) ) {
      x = coredata(data[i,])
      o = sort.list(x, na.last = TRUE, decreasing = dirMaxMin)
      index = which(!is.na(x))
      x[] = NA
      
      if(len(index)>0) {
        n = min(topn, len(index))
        x[o[1:n]] = 1
        
        # keepn logic
        if( i>=2 ) {
          y = coredata(out[(i-1),])	# previous period selection
          n1 = min(keepn, len(index))
          y[-o[1:n1]] = NA	# remove all not in top keepn
          
          index1 = which(!is.na(y))
          if(len(index1)>0) {
            x[] = NA
            x[index1] = 1	# keep old selection
            
            for( j in 1:n ) {
              if( sum(x, na.rm = T) == topn ) break
              x[o[j]] = 1
            }
          }
        }
      }		
      out[i,] = x/sum(x, na.rm = T)	
    }
    out[is.na(out)] = 0	
    return( out )
  }
}







#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')	
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ')	

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1970::2011')

#*****************************************************************
# Code Strategies
#****************************************************************** 
prices = data$prices  
n = len(tickers)  

# find month ends
month.ends = endpoints(prices, 'months')
month.ends = month.ends[month.ends > 0]		

# Equal Weight
data$weight[] = NA
data$weight[month.ends,] = ntop(prices, n)[month.ends,]	
capital = 100000
data$weight[] = (capital / prices) * data$weight
equal.weight = bt.run(data, type='share')


# Rank on 6 month return
position.score = prices / mlag(prices, 126)	

# Select Top 2 funds
data$weight[] = NA
data$weight[month.ends,] = ntop(position.score[month.ends,], 2)	
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)		
top2 = bt.run(data, type='share', trade.summary=T)

# Seletop Top 2 funds,  and Keep then till they are in 1:6 rank
data$weight[] = NA
data$weight[month.ends,] = ntop.keep(position.score[month.ends,], 2, 6)	
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)		
top2.keep6 = bt.run(data, type='share', trade.summary=T)

#*****************************************************************
# Create Report
#****************************************************************** 
plotbt.custom.report(top2.keep6, top2, equal.weight, trade.summary=T)