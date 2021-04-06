#------------------------------------------------------------------------------
#------------------- Quasi Poisson Data Sampling ------------------------------
#------------------------------------------------------------------------------

# n is the numbers of observations to draw
# lambda is the overall poisson mean
# phi is the dispersion parameter

rqpois <- function(n, lambda, phi){
        
        # Phi must be bigger than 1
        if(phi<=1){
                
                stop("phi<=1")
        }
        
        
        # Defining kappa following the parametrisation of the nb-distribution 
        # of Gsteiger et al 2013 (Stats in Med)
        kappa <- (phi-1)/lambda
        
        # Gamma parameters
        a <- 1/kappa
        b <- 1/(kappa*lambda)
        
        lambda_h <- rgamma(n=n, shape=a, rate = b)
        
        obs <- integer(length(lambda_h))
        
        for(h in 1:length(obs)){
                obs[h] <- rpois(n=1, lambda=lambda_h[h])
        }
        
        return(obs)
}







