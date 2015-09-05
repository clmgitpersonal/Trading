# First, we find a version of the formula for MVP that can be converted into those systems.
# I like the one used by Yue Kuen KWOK:
  
#  ω = Ω⁻¹ 1/ 1 ⋅ Ω⁻¹ 1

# where Ω is the covariant matrix of the returns for the stocks in question.

# Calculating returns can be done with standard matrix operations and slicing.
# The covariant function is built in, as is inverting it (solve). Since the
# numerator Ω⁻¹ 1 appears in the denominator, I reuse it's value there.

# All the array operations were documented in the same place. 
# That I only needed one unit vector was a bit of a surprise, 
# but R sized it dynamically to work. That I had to transpose
# the unit vector and use the cross product operator (%*%) to
# get a dot product was a a less pleasant surprise, but is
# apparently a standard R idiom.

minvar <- function (px){
  Rt <- px[-1,]/px[-nrow(px),]-1
  cov.Rt <- cov(Rt)
  one.vec <- rep(1,nrow(cov.Rt))
  num <- solve(cov.Rt) %*% one.vec  # solve inverts the matrix
  den <- as.numeric(t(one.vec) %*% num)
  return(num/den)
}