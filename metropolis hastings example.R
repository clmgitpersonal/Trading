
## Metropolis Hastings demonstration by Mike Flynn
## Warning: This code has a runtime of around 4 hours on my machine
samples = matrix(nrow = 10^6, ncol = 2)
currentSample = c(0.001,0.001)
set.seed(44)
for(i in 1:(10^8)) {
  ##  for(i in 1:(10^2)) {
  ## choose direction and distance for transition
  direction = runif(1, 0, 2*pi)
  distance = rexp(1, 2)
  
  ## current point
  x1 = currentSample[1]
  y1 = currentSample[2]
  
  ## compute next point
  x2 = x1 + distance*cos(direction) 
  y2 = y1 + distance*sin(direction)
  
  ## accept?
  accept = min(sin(sqrt(x2^2 + y2^2))^2*(x1^2 + y1^2)^(3/2)/(sin(sqrt(x1^2 + y1^2))^2*(x2^2 + y2^2)^(3/2)),1)
  if(accept > runif(1, 0, 1)) {
    currentSample = c(x2, y2)
  } else {
    currentSample = c(x1, y1)
  }
  ## only take 1 of every 100 to reduce autocorrelation
  if(i %% 100 == 0) {
    samples[i/100,] = currentSample
  }
}

library(ggplot2)
library(ggthemes)
plotdat = data.frame(x = samples[,1], y = samples[,2])
## display p to plot
p = ggplot(data = plotdat, aes(x = x, y=y)) + geom_point(alpha = .05, size =.01) + theme_bw() +
  xlim(-6*pi, 6*pi) + ylim(-6*pi, 6*pi)