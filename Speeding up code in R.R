#############################################################################################
### Load the necessary libraries for the class
require("microbenchmark")
require("compiler")
require("parallel")
require("doParallel")
require("foreach")
require("snow")
require("snowfall")

# Test the package microbenchmark
x <- runif(100)
sqrt(x)
x ^ 0.5

system.time(sqrt(x)) 
system.time(x^0.5) 

microbenchmark(sqrt(x), x ^ 0.5) 

#############################################################################################
### Extreme dynamism example
f <- function(x) NULL

s3 <- function(x) UseMethod("s3")
s3.integer <- f

A <- setClass("A", representation(a = "list"))
setGeneric("s4", function(x) standardGeneric("s4"))
setMethod(s4, "A", f)

B <- setRefClass("B", methods = list(rc = f))

a <- A()
b <- B$new()

# If this returns an error about negative time, just remove fun()=f from the call
microbenchmark(
  fun = f(),
  S3 = s3(1L),
  S4 = s4(a),
  RC = b$rc()
)

##############################################################################################
### Name lookup with mutable environments

# Example 1: Each time we print a, it comes from a different environment
a <- 1
f <- function() {
  g <- function() {
    print(a)
    assign("a", 2, envir = parent.frame())
    print(a)
    a <- 3
    print(a)
  }
  g()
}
f()

# Example 2: This illustrates the cost of looking for something through multiple environments
# This example comes from the Wickham book, but I cannot reproduce it. It might be that this is 
#  now improved in R 3.0.3 or something.

f <- function(x, y) {
  (x + y) ^ 2
}

random_env <- function(parent = globalenv()) {
  letter_list <- setNames(as.list(runif(26)), LETTERS)
  list2env(letter_list, envir = new.env(parent = parent))
}
set_env <- function(f, e) {
  environment(f) <- e
  f
}
f2 <- set_env(f, random_env())
f3 <- set_env(f, random_env(environment(f2)))
f4 <- set_env(f, random_env(environment(f3)))

microbenchmark(
  f(1, 2),
  f2(1, 2),
  f3(1, 2),
  f4(1, 2),
  times = 10000
)

##############################################################################################
### Lazy evaluation overhead

# As above, I cannot reproduce this example. Computers are much faster now than they were
#  during Wickham's time.

f0 <- function() NULL
f1 <- function(a = 1) NULL
f2 <- function(a = 1, b = 1) NULL
f3 <- function(a = 1, b = 2, c = 3) NULL
f4 <- function(a = 1, b = 2, c = 4, d = 4) NULL
f5 <- function(a = 1, b = 2, c = 4, d = 4, e = 5) NULL
microbenchmark(f0(), f1(), f2(), f3(), f4(), f5(), times = 1000)
  
##############################################################################################
### R implementation example
# All these methods should be as fast as each other, but some are clearly faster than others

head(mtcars)
microbenchmark(
  mtcars[32, 11],
  mtcars[[c(11, 32)]],
  mtcars[[11]][32],
  mtcars$carb[32],
  .subset2(mtcars, 11)[32]
)

# Another example showing two known slow functions: ifelse and pmin/pmax
squish_ife <- function(x, a, b) {
  ifelse(x <= a, a, ifelse(x >= b, b, x))
}
squish_p <- function(x, a, b) {
  pmax(pmin(x, b), a)
}
squish_in_place <- function(x, a, b) {
  x[x <= a] <- a
  x[x >= b] <- b
  x
}

x <- runif(100, -1.5, 1.5)
microbenchmark(
  squish_ife(x, -1, 1),
  squish_p(x, -1, 1),
  squish_in_place(x, -1, 1)
)

# EXERCISES: 
# Compare the performance of extracting a column from a matrix, a data frame and a list
#     How about a row? How do you think this could be improved?
# Compare the performance of the Squish functions for different sizes of x.

###########################################################################################
### Profiling with Rprof

# Functions which form a matrix of powers of the vector x, through degree dg
# powers1() grows objects (more on this later)
powers1 <- function(x,dg){
  pw <- matrix(x,nrow=length(x))
  prod <- x  # current product
  for(i in 2:dg){
    prod <- prod * x
    pw <- cbind(pw, prod)
  }
  return(pw)
}

# powers2() does the same thing, but declares the objects in advance
powers2 <- function(x,dg){
  pw <- matrix(nrow=length(x), ncol=dg)
  prod <- x  # current product
  pw[,1] <- prod
  for(i in 2:dg){
    prod <- prod * x
    pw[,i] <- prod
  }
  return(pw)
}

# Compare the two with microbenchmark
x <- runif(1e4)
microbenchmark(powers1(x,40),
               powers2(x,40))

# Use Rprof() to find bottlenecks
x <- runif(1e6)
Rprof()
invisible(powers1(x,30))
Rprof(NULL)
summaryRprof()

Rprof()
invisible(powers2(x,30))
Rprof(NULL)
summaryRprof()

# EXERCISE (optional):
# Write the 'powers' function using outer() and cumprod(). How do these vectorized functions
#     compare with the options above?

###########################################################################################
### Vectorizing
# A trivial example
for_sum <- function(x){
  y <- numeric()
  for(i in 1:length(x))
    y <- y + x[i]
  y
}

x <- runif(1000)
microbenchmark(for_sum(x), sum(x))

# An easy example using if and ifelse
for_if <- function(x){
  y <- numeric(length(x))
  for(i in 1:length(x))
    if(x[i] < 0) y[i] <- 0
  y
}

x <- runif(10000)
microbenchmark(for_if(x), ifelse(x<0, 0, x))

# Apply is not really much faster than a for loop, and much slower than a truly vectorized function
for_fun <- function(x){
  y <- numeric(dim(x)[1])
  for(i in 1:dim(x)[1])
    y[i] <- sum(x[,1])
  y
}

apply_fun <- function(x){
  y <- apply(x,2,sum)
  y
}

x <- matrix(runif(500^2),ncol=500)
microbenchmark(for_fun(x), apply_fun(x), colSums(x))

# EXERCISES:
#  How do these methods compare as the size of x changes?
#  What about other functions from the apply family? 

#################################################
# Over-vectorizing

# Higher-order functions for functional programming can be great when developing packages,
#  but they do not improve time, and can even be slower
sum_fun <- function() {
  s <- x[1] + x[2]
  for (i in 3:length(x)) s <- s + x[i]
  s
}

red_fun <- function(a,b) a + b

microbenchmark(sum_fun(),Reduce(red_fun,x),cumsum(x))

###########################################################################################
### Do less work

# Giving a function more information can make a large difference. For example, giving
#   factor() the levels in advance.
letter_stuff <- sample(letters, size=1e5, replace=TRUE)

factor_a <- function(x) factor(x)
factor_b <- function(x) factor(x, levels=letters)

microbenchmark(factor_a(letter_stuff), factor_b(letter_stuff))

# Avoiding method dispatch can save a lot of time, but it is dangerous. If you give it bad input
# it won't know what to do and can fail in spectacular ways
quickdf <- function(l) {
  class(l) <- "data.frame"
  attr(l, "row.names") <- .set_row_names(length(l[[1]]))
  l
}

l <- lapply(1:26, function(i) runif(1e3))
names(l) <- letters

microbenchmark(quickdf(l), as.data.frame.list(l), as.data.frame(l))

quickdf(list(x = 1, y = 1:2)) # This is going to fail

###########################################################################################
### Memory, growing objects

# Growing objects is very slow.
grow_obj <- function(x){
  y <- numeric()
  for(i in 1:x) y <- c(y,i)
  y
}
subscript_obj <- function(x){
  y <- numeric(x)
  for(i in 1:x) y[i] <- i
  y
}
colon_obj <- function(x){
  y <- 1:x
  y
}

microbenchmark(grow_obj(50), grow_obj(500), grow_obj(5000),
  subscript_obj(50), subscript_obj(500), subscript_obj(5000),
  colon_obj(50), colon_obj(500), colon_obj(5000)
  )

###########################################################################################
### Byte code compilation

x <- runif(1e4)
y <- runif(1e4)
z <- numeric(1e4)

sum_f <- function() for(i in 1:length(x)) z[i] <<- x[i] + y[i]
csum_f <- cmpfun(sum_f)

microbenchmark(x+y,sum_f(),csum_f())

###########################################################################################
### Parallelization
# A matrix inversion example for parallelizing

x <- 1700
mat <- matrix(runif(x^2),ncol=x)
invert_mat <- function(mat){
  y <- solve(mat)
  y
}
# Check more or less how long does a single instance of invert_mat takes
system.time(invert_mat(mat))

#==================================================
#Example of multiple matrix inversions
# Doing matrix inversion for multiple 

x <- 1700  #Dimensions of matrix to create and invert
n <- 10  #Number of times to create matrix and do matrix inversion
series <- 1:n


###############################################
# USING A LOOP - Slow (sad face)
loop_output <- list(n)

time_loop <- FALSE
time_loop <- system.time({
  for(i in series){
    mat <- matrix(runif(x^2), ncol=x)
    loop_output[[i]] <- invert_mat(mat)
  }
})


###############################################
#USING foreach IN foreach LIBRARY

# foreach() replaces a for loop directly, 
#  this simplifies things as you don't need to write a wrapper funciton.

time_foreach <- FALSE
time_foreach <- system.time({
  foreach_func <- function(i, r) {
    mat <- matrix(runif(r^2), ncol=r)
    temp_output <- invert_mat(mat)
    return(temp_output)
  }
  
  # Registering the doParallel backend
  cl <- makeCluster(4)
  registerDoParallel(cl)
  
  # Call foreach
  #  Note that i and r have the same length
  foreach_output <- foreach(i=series, r=rep(x,x)) %dopar%
    foreach_func(i, r)
  
  # Stop the cluster
  stopCluster(cl=NULL)
  # This step is VERY IMPORTANT. According to the internet, not stopping a cluster
  #  could harm your computer.
})


################################################
#USING sfLapply IN snowfall LIBRARY
time_snowfall <- FALSE
time_snowfall <- system.time({
  
  r <- x   # snowfall gets angry if you pass it something called 'x'
  
  snowfall_func <- function(i, r=r) {
    mat <- matrix(runif(r^2),ncol=r)
    inv_mat <- invert_mat(mat)
    return(inv_mat)
  }
  
  sfInit(parallel=TRUE, cpus=4, type='SOCK')
  
  # After initializing the snowfall instance you must import all data objects 
  #  and functions needed
  
  # If you want to import everything in the current session, you can use:
  # sfExportAll() # This can be very slow
  
  # Otherwise, you can import only the invert_mat() function specified above
  sfExport('invert_mat')
  
  snow_output <- sfLapply(series, fun=snowfall_func, r=r)
  #output_snowfall <- unlist(rbind(output))
  
  sfStop()
  
})


###############################################
# USING mclapply IN parallel LIBRARY
# IMPORTANT: This won't do much if you're on a Windows machine!
?mclapply

time_mclapply <- FALSE
time_mclapply <- system.time({
  
  #For mclapply you must create a wrapper function
  #  which is executed at each stage in the loop above
  #    i.e. the wrapper function is evaluated for each value of "series"
  
  mclapply.func <- function(i, x=x) {
    mat <- matrix(runif(x^2),ncol=x)
    inv.mat <- invert_mat(mat)
    return(inv.mat)
  }
  
  #Use the detectCores function in "parallel" library to determine number of cores on your computer
  n.cores <- detectCores()
  print(paste("### CONGRATULATIONS YOU HAVE:", n.cores, "CORES ON YOUR COMPUTER"))
  
  
  output <- mclapply(series, FUN=mclapply.func, x=x, mc.cores=n.cores)
  
  #Output is in list form... therefore we need to reconstruct it back into a 3d-array
  mclapply_output <- array(data=unlist(output), dim=c(x,x,n))
  
  #Alternatively the the list output can be reconstructed into
  #mclapply.output.2 <- unlist(rbind(output))
  
})


print(time_loop)
print(time_foreach)
print(time_snowfall)
print(time_mclapply)

# EXERCISE: 
# Compare the scalability of all approaches. How does system.time vary as you change 
#   the values for n and x? Remember to check first how long does it take to run a single
#   instance of inv_mat. Run time does not increase linearly with x. 
# If you're feeling brave, also try changing the number of cores you're using. 