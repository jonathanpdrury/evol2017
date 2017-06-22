###THIS VERSION uses input formatted the same way as in EvoRAG, forces S to take negative values


logLH_MCsis<-function(DIST,TIME, pars){

	Vi<-MC_variance_sis(times=TIME, pars=c(b=exp(pars[1]),s=-abs(pars[2])))$variance  ##first, calculate Vi for given pars and times
	lnPi<-log(exp(-(DIST^2)/(2*Vi)))-0.5*log(2*pi*Vi)
	
	if(is.infinite(-sum(lnPi))){
		return(10000)
		} else{
		return(-sum(lnPi))
		}
}

logLH_MClin_sis<-function(DIST,TIME,GRAD,pars){
	Vi<-MC_lin_variance_sis(times=TIME, gradient=GRAD, pars=c(b=exp(pars[1]),s0=-abs(pars[2]),sb=-abs(pars[3])))$variance  ##first, calculate Vi for given pars and times
	lnPi<-log(exp(-(DIST^2)/(2*Vi)))-0.5*log(2*pi*Vi)
	if(is.infinite(-sum(lnPi))){
		return(10000)
		} else{
		return(-sum(lnPi))
		}
}

#this is a model where non-IT species (gradient=0) are forced to evolve via BM; slope is unconstrained
logLH_MClin_sis_zerointercept<-function(DIST,TIME,GRAD,pars){
	Vi<-MC_lin_variance_sis(times=TIME, gradient=GRAD, pars=c(b=exp(pars[1]),s0=0,sb=pars[3]))$variance  ##first, calculate Vi for given pars and times
	lnPi<-log(exp(-(DIST^2)/(2*Vi)))-0.5*log(2*pi*Vi)
	if(is.infinite(-sum(lnPi))){
		return(10000)
		} else{
		return(-sum(lnPi))
		}
}

logLH_BMsis<-function(DIST,TIME, pars){
	Vi<-MC_variance_sis(times=TIME, pars=c(b=exp(pars[1]),s=0))$variance  ##first, calculate Vi for given pars and times
	lnPi<-log(exp(-(DIST^2)/(2*Vi)))-0.5*log(2*pi*Vi)
	if(is.infinite(-sum(lnPi))){
		return(10000)
		} else{
		return(-sum(lnPi))
		}
}

logLH_BMlin_sis<-function(DIST,TIME,GRAD,pars){
	Vi<-BM_lin_variance_sis(times=TIME, gradient=GRAD, pars=c(b=exp(pars[1]),s0=0,sb=pars[3]))$variance  ##first, calculate Vi for given pars and times
	lnPi<-log(exp(-(DIST^2)/(2*Vi)))-0.5*log(2*pi*Vi)
	if(is.infinite(-sum(lnPi))){
		return(10000)
		} else{
		return(-sum(lnPi))
		}
}
