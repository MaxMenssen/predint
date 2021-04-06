
pi_rho_est <- function(dat){
        
        if(ncol(dat) != 2){
                stop("ncol(dat)!=2, rho can not be estimated")
        }
        
        # Calculation of the proportion
        if(all(dat[,1]==0))
        {
                dat[,1][1] <- 0.5
                dat[,2][1] <- dat[,2][1]-0.5
        }
        #
        
        if(all(dat[,2]==0))
        {
                dat[,2][1] <- 0.5
                dat[,1][1] <- dat[,1][1]-0.5
        }
        # 
        
        Yj <- dat[,1]
        mj <- rowSums(dat)
        k <- nrow(dat)
        
        pi_hat <- sum(Yj)/sum(mj)
        
        
        ### Intra class correlation nach Lui et al 2000
        
        # Between mean squared error
        part1B <- sum(Yj^2/mj)
        part2B <- (sum(Yj)^2/sum(mj))
        part3B <- k-1
        
        BMS <- (part1B-part2B)/part3B
        
        
        # Within mean squared error
        part1W <- sum(Yj)
        part2W <- sum(Yj^2/mj)
        part3W <- sum(mj-1)
        
        WMS <- (part1W-part2W)/part3W
        
        
        # mstar
        part1m <- sum(mj)^2
        part2m <- sum(mj^2)
        part3m <- (k-1)*sum(mj)
        
        mstar <- (part1m-part2m)/part3m
        
        
        # Estimated intrasclass correlation
        rho_hat <- (BMS-WMS)/(BMS+(mstar-1)*WMS)
        
        out <- c("pi_hat"=pi_hat, "rho_hat"=rho_hat)
        
        
        return(out)
}






