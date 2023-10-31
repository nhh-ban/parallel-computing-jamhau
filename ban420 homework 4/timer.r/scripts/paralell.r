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

# Register the number of cores to be used for parallel computation
# This example uses 8 cores. 
maxcores <- 8
Cores <- min(parallel::detectCores(), maxcores)
cl <- makeCluster(Cores)
registerDoParallel(cl)

df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 

# Export necessary functions and data to the cluster workers:
clusterExport(cl, list("MTweedieTests", "simTweedieTest"))

tic(paste0("Parallel loop for MTweedieTests, ", Cores, " cores"))

for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
} 

toc()

#### Soulution 
#### with paralall comuting with 8 cores i get: 
#### Parallel loop for MTweedieTests, 8 cores: 25.079 sec elapsed



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
