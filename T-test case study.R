#####################################################################################
### T-test case study
# From Hadley Wickham, Advanced R Programming

# Let's say we want to run a bunch of t-tests, comparing data from 1000 experiments on 50
#  individuals, half of which belong to group 1 and the other half to group 2.
m <- 1000 # Number of experiments
n <- 50   # Number of individuals
X <- matrix(rnorm(m * n, mean = 10, sd = 3), nrow = m)
grp <- rep(1:2, length = n)

# Using the base R t.test function with a formula interfase is slow
system.time(for(i in 1:m) t.test(X[i, ] ~ grp)$stat)
# Using subsetting to provide two vectors is faster.
system.time(for(i in 1:m) t.test(X[i, grp == 1], X[i, grp == 2])$stat)

# How about replacing the for with an apply?
compT <- function(x, grp){
  t.test(x[grp == 1], x[grp == 2])$stat
}
system.time(apply(X, 1, compT, grp = grp))
# Hmm, not much improvement. 

# If you look at the t.test source code*, you can see the function does a bunch of stuff that
#  we don't need in this case, so we can go ahead and simplify the function to make it faster.
#  Let's start by removing p-values and and the creation of nice outputs for printing. 
#  (*You can see the source code at https://github.com/wch/r-source/blob/trunk/src/library/stats/R/t.test.R)
my_t <- function(x, grp) {
  t_stat <- function(x) {
    m <- mean(x)
    length <- length(x)
    var <- sum((x - m) ^ 2) / (n - 1)
    
    list(m = m, n = n, var = var)
  }
  
  g1 <- t_stat(x[grp == 1])
  g2 <- t_stat(x[grp == 2])
  
  pooled_se <- sqrt(g1$var / g1$n + g2$var / g2$n)
  (g1$m - g2$m) / pooled_se
}
system.time(apply(X, 1, my_t, grp = grp))
# About a 5x speedup

# Next, it could be vectorized by changing the function so that it works with a matrix instead of a vector.
#  mean() becomes rowMeans(), length() becomes ncol() and sum() becomes rowSums().
rowtstat <- function(X, grp){
  t_stat <- function(X) {
    m <- rowMeans(X)
    n <- ncol(X)
    var <- rowSums((X - m) ^ 2) / (n - 1)
    
    list(m = m, n = n, var = var)
  }
  
  g1 <- t_stat(X[, grp == 1])
  g2 <- t_stat(X[, grp == 2])
  
  pooled_se <- sqrt(g1$var / g1$n + g2$var / g2$n)
  (g1$m - g2$m) / pooled_se
}
system.time(rowtstat(X, grp))
# That's much faster!

# What if we now use byte compiling?
rowtstat_bc <- compiler::cmpfun(rowtstat)

microbenchmark(
  rowtstat(X, grp),
  rowtstat_bc(X, grp)
)
# Sad face, that didn't do much. 