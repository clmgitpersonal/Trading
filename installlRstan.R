# Setup for RStan

Sys.setenv(MAKEFLAGS = "-j4")   # Set number of cores for build
source('http://mc-stan.org/rstan/install.R', echo = TRUE, max.deparse.length = 2000)
install_rstan()

# Testing Rcpp
library(Rcpp)
cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')
add(1,2,3)

