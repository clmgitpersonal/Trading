symbols <- c("AAPL", "AMZN", "AXP", "BA", "BAC", "CAJ", "CAT", "CL", "CMCSA",
            "COP", "CSCO", "CVC", "CVS", "CVX", "DD", "F", "GD", "GE", "GS",
            "GSK", "HD", "HMC", "HPQ", "IBM", "JPM", "K", "KMB", "KO", "LMT",
            "MAR", "MCD", "MDLZ", "MMM", "MSFT", "MTU", "NAV", "NOC", "NVS",
            "PEP", "PFE", "PG", "R", "RTN", "SAP", "SNE", "SNY", "TM", "TOT",
            "TWX", "TXN", "UN", "VLO", "WBA", "WFC", "WMT", "XOM", "XRX", "YHOO")

sectors           <- c("consumer", "energy", "finance", "industrial", "pharma", "tech")

sector_assignment <- factor(sectors[c(6,1,3,3,3,6,4,1,1,2,6,1,1,2,4,1,4,4,3,5,1,1,6,6,3,1,1,1,4,1,
                                      1,1,4,6,3,4,4,5,1,5,1,4,4,6,6,5,1,2,1,6,1,2,1,3,1,2,6,6)])

library(quantmod)
p       <- lapply(symbols, function(n) {print(n);getSymbols(n, auto.assign=FALSE)[,4]})
x       <- Reduce(cbind, p)
returns <- apply(x,2,function(z) diff(log(z)))

library("corpcor")
Sr <- cor.shrink(returns,lambda=0.5)             # shrink

## Specified shrinkage intensity lambda (correlation matrix): 0.5

Pr <- solve(Sr,diag(rep(1,nrow(Sr))))            # invert
Qr <- Pr*(abs(Pr)>quantile(abs(Pr),probs=0.9))   # threshold
colnames(Qr) <- rownames(Qr) <- symbols

library(networkD3)
edges <- which(Qr!=0, arr.ind=TRUE) # Adjaceny graph edge list
links <- data.frame(source=symbols[edges[,2]], target=symbols[edges[,1]])

# Let's color the vertices by stock sector.
names(sector_assignment) <- symbols
N <- length(levels(sector_assignment))
sector_palette <- substr(rainbow(N), 1, 7)
vertex_colors <- sector_palette[as.integer(sector_assignment[unique(Reduce(c,t(links)))])]

simpleNetwork(links, fontSize=16, textColour="#000011",
              linkColour="#bbbbbb", nodeColour=vertex_colors,
              charge=-250, nodeClickColour=NULL)