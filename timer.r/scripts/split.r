# Assignment 1:  
library(tweedie) 
library(ggplot2)
library(tictoc)
library(knitr)


tic()
simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 
toc()


# Assignment 2:  

tic()
MTweedieTests <-  
  function(N,M,sig){ 
    sum(replicate(M,simTweedieTest(N)) < sig)/M 
  } 
toc()

# Assignment 3:  
#Load package
library(doParallel)

# Parallelized MTweedieTests
MTweedieTests_parallel <- function(N, M, sig){
  results <- foreach(m = 1:M, .combine = c, .packages = "tweedie") %dopar% {
    simTweedieTest(N) < sig
  }
  
  return(sum(results) / M)
}


# Register the number of cores to be used for parallel computation
# This example uses 8 cores. 
maxcores <- 8
Cores <- min(parallel::detectCores(), maxcores)
cl <- makeCluster(Cores)
registerDoParallel(cl)



clusterExport(cl, list("MTweedieTests_parallel", "simTweedieTest"))

df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 

tic(paste0("Parallel loop for MTweedieTests, ", Cores, " cores"))
for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MTweedieTests_parallel( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
} 
toc()

# Close the cluster once done
stopCluster(cl)

#### 2.3 - Soulution 
#### When splitting the m simulations in more than one core for the  MTweedieTests 
# we get a runtime of 5.04 seconds on 8 cores. 
# Which is drasticly faster than the 2 previos methods. 

## Comparison: 
# For 2.1 the time it takes to run the assigment 3 function is: 25.398 sec elapsed
# for 2.2 the time is 25.079 sec 
#for 2.3 the time is 5.04 sec
#The reason why its faster might be simmultaneous execution. ultiple tasks can run concurrently,
#which means multiple operations can be completed at the same time. 
# Also Better Utilization of Resources: Modern computers have multiple cores. 
#When tasks are run sequentially on a single core, the remaining cores remain underutilized. 
#Parallel computing utilizes these cores, which means more tasks can be done in the same amount of time.



## Assignemnt 4 
   
# This is one way of solving it - maybe you have a better idea? 
# First, write a function for simulating data, where the "type" 
# argument controls the distribution. We also need to ensure 
# that the mean "mu" is the same for both distributions. This 
# argument will also be needed in the t-test for the null 
# hypothesis. Therefore, if we hard code in a value here 
# we may later have an inconsistency between the mean of the 
# distributions and the t-test. So, we add it as an explicit 
# argument:  


library(magrittr)
library(tidyverse)

tic()
simDat <-
  function(N, type, mu) {
    if (type == "tweedie") {
      return(rtweedie(
        N,
        mu = mu,
        phi = 100,
        power = 1.9
      ))
    }
    if (type == "normal") {
      return(rnorm(N, mean = mu))
    }
    else{
      stop("invalid distribution")
    }
  }

toc()
# Next, the test. Note, we use mu two places:
# both for the data simulation and as the null.

tic()
simTest <-
  function(N, type, mu) {
    t.test(simDat(N = N,
                  type = type,
                  mu = mu),
           mu = mu)$p.value
  }
toc()

# Running many tests is almost the same as before.
# Here the mean is hard coded in, as we're not
# going to change it.

tic()
Tests <-
  function(N, M, type, sig) {
    sum(replicate(M,
                  simTest(
                    N = N,
                    type =
                      type,
                    mu =
                      10000
                  )) < sig) / M
  }

toc()
# We can now repeat the same analysis as before,
# but for both the tweedie and the normal:
df <-
  expand.grid(
    N = c(10, 100, 1000, 5000),
    M = 1000,
    type = c("tweedie", "normal"),
    share_reject = NA
  ) %>%
  as_tibble()

tic()
for (i in 1:nrow(df)) {
  print(i)
  df$share_reject[i] <-
    MTests(df$N[i],
           df$M[i],
           df$type[i],
           .05)
}
toc()
