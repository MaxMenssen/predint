#-------------------------------------------------------------------------------
#--- Helper function to assess the coverage probability (used in bisection) ----
#-------------------------------------------------------------------------------

# Function to assess the coverage probability
coverage_prob <- function(y_star_hat,
                          pred_se,
                          q,
                          y_star,
                          alternative){

        # y_star_hat has to be a list
        if(!is.list(y_star_hat)){
                stop("!is.list(y_star_hat)")
        }

        # pred_se has to be a list
        if(!is.list(pred_se)){
                stop("!is.list(pred_se)")
        }

        # y_star has to be a list
        if(!is.list(y_star)){
                stop("!is.list(y_star)")
        }

        # all three lists have to have the same length
        if(length(unique(c(length(y_star_hat), length(pred_se), length(y_star)))) != 1){
                stop("length(unique(c(length(y_star_hat), length(pred_se), length(y_star)))) != 1")
        }

        # q needs to be numeric
        if(!is.numeric(q)){
                stop("!is.numeric(q)")
        }

        # alternative must be defined
        if(isTRUE(alternative!="both" && alternative!="lower" && alternative!="upper")){
                stop("alternative must be either both, lower or upper")
        }


        #-----------------------------------------------------------------------
        # q times pred_se
        q_se_list <- mapply(FUN = function(x,y){x*y},
                            x = pred_se,
                            MoreArgs = list(y=q),
                            SIMPLIFY=FALSE)

        if(alternative=="both"){

                # Lower border
                lower_list <- mapply(FUN = function(x,y){x-y},
                                     x = y_star_hat,
                                     y = q_se_list,
                                     SIMPLIFY=FALSE)

                # Upper border
                upper_list <- mapply(FUN = function(x,y){x+y},
                                     x = y_star_hat,
                                     y = q_se_list,
                                     SIMPLIFY=FALSE)

        }

        if(alternative=="lower"){

                # Lower border
                lower_list <- mapply(FUN = function(x,y){x-y},
                                     x = y_star_hat,
                                     y = q_se_list,
                                     SIMPLIFY=FALSE)
        }

        if(alternative=="upper"){

                # Upper border
                upper_list <- mapply(FUN = function(x,y){x+y},
                                     x = y_star_hat,
                                     y = q_se_list,
                                     SIMPLIFY=FALSE)

        }


        # Function to check the coverage
        cover_fun <- function(lower=NULL,
                              upper=NULL,
                              y_star,
                              alternative){

                if(alternative=="both"){

                        # If  all y_star are covered set output to 1
                        if(all(lower <  y_star & y_star < upper)){
                                return(1)
                        }

                        # If not all y_star are covered set output to 0
                        if(!all(lower <  y_star & y_star < upper)){
                                return(0)
                        }
                }

                if(alternative=="lower"){

                        # If  all y_star are covered set output to 1
                        if(all(lower <  y_star)){
                                return(1)
                        }

                        # If not all y_star are covered set output to 0
                        if(!all(lower <  y_star )){
                                return(0)
                        }
                }

                if(alternative=="upper"){

                        # If  all y_star are covered set output to 1
                        if(all(y_star < upper)){
                                return(1)
                        }

                        # If not all y_star are covered set output to 0
                        if(!all(y_star < upper)){
                                return(0)
                        }
                }
        }


        if(alternative=="both"){

                cover_list <- mapply(FUN = cover_fun,
                                     lower=lower_list,
                                     upper=upper_list,
                                     y_star=y_star,
                                     MoreArgs = list(alternative="both"),
                                     SIMPLIFY=FALSE)
        }

        if(alternative=="lower"){

                cover_list <- mapply(FUN = cover_fun,
                                     lower=lower_list,
                                     y_star=y_star,
                                     MoreArgs = list(alternative="lower"),
                                     SIMPLIFY=FALSE)
        }

        if(alternative=="upper"){

                cover_list <- mapply(FUN = cover_fun,
                                     upper=upper_list,
                                     y_star=y_star,
                                     MoreArgs = list(alternative="upper"),
                                     SIMPLIFY=FALSE)
        }


        cover_prob <- sum(unlist(cover_list))/length(cover_list)

        return(cover_prob)

}
