#-------------------------------------------------------------------------------
#----------------- Sampling of quasi-binomial data -----------------------------
#-------------------------------------------------------------------------------

# Generation of historical data set with different n_k

# n is the number of clusters
# size is the total number of observations in each cluster
# n and size have the same function as in rbinom

rbbinom <- function(n, size, prob, rho){
        
        # n must be of length 1
        if(length(n) != 1){
                stop("length(n) must be 1")
        }
        
        # Rho must be bigger than 0
        if(rho<=0){
                stop("rho<=0") 
        }
        
        # Same clustersise for all clusters (one size given)
        if(length(size)==1){
                nhist <- rep(size, n)
        }
        
        # Severeal cluster sizes
        else if(length(size) == n){
                nhist <- size
        }
        
        # If size and n do not match, stop
        else{
                stop("length(size) and n do not match")
        }
        
        
        # Beta parameters with fixed rho and asum
        asum <-(1-rho)/rho
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

#------------------------------------------------------------------------------









