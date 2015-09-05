library("rjags")
set.seed(1337)

y<-rnorm(n = 20, mean = 10, sd = 5)
mean(y)
sd(y)
model_string <- 
"model{

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



schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = '8schools.stan', data = schools_dat, 
            iter = 1000, chains = 4)


# 
# This is a demo of using model_code argument since 
# we can use this file directly or put the string in R 
# directly. 
#  
schools_code <- paste(readLines('8schools.stan'), collapse = '\n')
fit1 <- stan(model_code = schools_code, data = schools_dat, 
             iter = 1000, chains = 4)

fit2 <- stan(fit = fit1, data = schools_dat, iter = 10000, chains = 4)

print(fit2)
plot(fit2)

la <- extract(fit2, permuted = TRUE) # return a list of arrays 
mu <- la$mu 

### return an array of three dimensions: iterations, chains, parameters 
a <- extract(fit2, permuted = FALSE) 

### use S3 functions as.array (or as.matrix) on stanfit objects
a2 <- as.array(fit2)
m <- as.matrix(fit2)

print(fit, digits = 1)

notFoolFunds <- data.frame(read.cb())