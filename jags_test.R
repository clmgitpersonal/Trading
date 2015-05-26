library("rjags")
set.seed(1337)

y<-rnorm(n = 20, mean = 10, sd = 5)
mean(y)
sd(y)
model_string <- "model{
for(i in 1:length(y)) {
y[i] ~ dnorm(mu, tau)
}
mu ~ dnorm(0, 0.0001)
sigma ~ dlnorm(0, 0.0625)
tau <- 1 / pow(sigma, 2)
}"
model <- jags.model(textConnection(model_string), data = list(y = y), n.chains = 3, n.adapt= 10000)
update(model, 10000) # Burnin for 10000 samples
mcmc_samples <- coda.samples(model, variable.names=c("mu", "sigma"), n.iter=20000)




library(Rcpp)
cppFunction('
  int one() {
    return 1;
  }
')
one()
