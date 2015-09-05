## JAGS LDA Model:

model {

## per-doc topic weights
for (i in 1:M) {
theta[i,1:K] ~ ddirch(alpha)
}

## per-topic word weights
for (i in 1:K) {
phi[i,1:V] ~ ddirch(beta)
}

## flattened docs
for (i in 1:W) {
z[i] ~ dcat(theta[D[i],])
w[i] ~ dcat(phi[z[i],])
}

}

Fully observed variables:
D K M V W alpha beta w
