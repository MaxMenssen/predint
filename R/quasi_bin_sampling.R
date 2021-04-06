#-------------------------------------------------------------------------------
#----------------- Sampling of quasi-binomial data -----------------------------
#-------------------------------------------------------------------------------

# Generation of historical data set with different n_k

# n is the number of historical clusters (formerly anzhist)
# size is the total number of observations in each cluster
# n and size have the same function as in rbinom

rqbinom <- function(n, size, prob, phi){
        
        # n must be of length 1
        if(length(n) != 1){
                stop("length(n) must be 1")
        }
        
        # If phi <= 1 stop
        if(phi<=1){
              stop("phi<=1") 
        }
        
        # clustersize must be >1
        if(min(size)<=1){
                stop("clustersize must be >1")
        }
        
        # If phi is not smaller than all cluster sizes, stop
        if(min(size) <= phi){
                stop("phi must be < size")
        }
        
        # All clusters have the same size
        if(length(size)==1){
                nhist <- rep(size, n)
        }
        
        # Vector of sizes
        else if(length(size) == n){
                nhist <- size
        }
        
        else{
                stop("length(size) and n do not match")
        }
        
        # Beta parameters with fixed phi
        asum <- (phi-nhist)/(1-phi) 
        a <- prob*asum 
        b <- asum-a
        
        # Sampling proportions from the beta distribution
        pis <- rbeta(n=n, shape1=a, shape2=b) 
        
        # Sampling observations from binomial distribution
        y <- rbinom(n=n, size=nhist, prob=pis)
        
        # Defining the output data
        x <- nhist-y
        dat <- data.frame(succ=y, fail=x)
        
        return(dat)
}






