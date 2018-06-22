fit_carbayes <- function(d, W, s="pc_district", t="t", mcmc=list(burnin=10, samples=1010, thin=10),verbose=FALSE){

    logger = function(...){
        if(verbose){
            message(...)
        }
    }
    
    s = d[[s]]
    t = d[[t]]

    us = unique(s)
    logger("Number of spatial locations: ",length(us))
    ut = unique(t)
    logger("Number of time points:       ",length(ut))

    units = list(s = us, t=ut)
    return(units)
    if(length(us)*length(ut) != nrow(d)){
        stop("Time and space coordinates do not form a grid")
    }
    tc = expand.grid(s = us, t = ut) # this is what it should be...

    if(!all(tc$t == t)){
        stop("Data not sorted in increasing time.")
    }
    if(!all(tc$s == s)){
        stop("Data not in consistent spatial order within time slices.")
    }
    logger("Data space and time in correct order.")

    
    car = ST.CARanova(Freq~offset(log(pop)), family="poisson", data=d, W=W, burnin=mcmc$burnin, n.sample=mcmc$samples, thin=mcmc$thin)
    car$units = units
    car
}


### CARBayesST naming:
###
###   beta : explanatory variables
###    phi : spatial effects
###  delta : temporal effects
###  gamma : fitted interactions - "anomalies"
###   tau2 : variances for phi, delta, gamma
###    rho : spatial/temporal correlation
### fitted : fitted values
###      Y : mcmc parameters


most_recent_effects <- function(cbfit){
### last N_s gamma values
    Ns = ncol(cbfit$samples$phi)
    Nt = ncol(cbfit$samples$delta)
    last_Ns = (1:Ns) + (Ns*(Nt-1))
    cbfit$samples$gamma[,last_Ns]
}
