###THIS VERSION uses input formatted the same way as in EvoRAG, forces S to take negative values


logLH_MCsis<-function(DIST,TIME, pars){

	Vi<-MC_variance_sis(times=TIME, pars=c(b=exp(pars[1]),s=-abs(pars[2])))$variance  ##first, calculate Vi for given pars and times using numerical integration
	#lnPi<-log(exp(-(DIST^2)/(Vi))/Vi)-log(pi)-log(2) #unsure why eqn4 is squared, this gives incorrect lh; formula below is correct
	lnPi<-log(exp(-(DIST^2)/(2*Vi)))-0.5*log(2*pi*Vi)
	
	if(is.infinite(-sum(lnPi))){
		return(10000)
		} else{
		return(-sum(lnPi))
		}
}

logLH_MClin_sis<-function(DIST,TIME,GRAD,pars){
	Vi<-MC_lin_variance_sis(times=TIME, gradient=GRAD, pars=c(b=exp(pars[1]),s0=-abs(pars[2]),sb=-abs(pars[3])))$variance  ##first, calculate Vi for given pars and times using numerical integration
	#lnPi<-log(exp(-(DIST^2)/(Vi))/Vi)-log(pi)-log(2) #unsure why eqn4 is squared, this gives incorrect lh; formula below is correct
	lnPi<-log(exp(-(DIST^2)/(2*Vi)))-0.5*log(2*pi*Vi)
	if(is.infinite(-sum(lnPi))){
		return(10000)
		} else{
		return(-sum(lnPi))
		}
}

#this is a model where non-IT species (gradient=0) are forced to evolve via BM; slope is unconstrained
logLH_MClin_sis_zerointercept<-function(DIST,TIME,GRAD,pars){
	Vi<-MC_lin_variance_sis(times=TIME, gradient=GRAD, pars=c(b=exp(pars[1]),s0=0,sb=pars[3]))$variance  ##first, calculate Vi for given pars and times using numerical integration
	#lnPi<-log(exp(-(DIST^2)/(Vi))/Vi)-log(pi)-log(2) #unsure why eqn4 is squared, this gives incorrect lh; formula below is correct
	lnPi<-log(exp(-(DIST^2)/(2*Vi)))-0.5*log(2*pi*Vi)
	if(is.infinite(-sum(lnPi))){
		return(10000)
		} else{
		return(-sum(lnPi))
		}
}




###BM functions just to make sure things are behaving as in EvoRAG
###NOTE that the LH that is acheived, and parameter values, are all exactly 2X the ones from EvoRAG (divided by two because of two branches)
logLH_BMsis<-function(DIST,TIME, pars){
	Vi<-MC_variance_sis(times=TIME, pars=c(b=exp(pars[1]),s=0))$variance  ##first, calculate Vi for given pars and times using numerical integration
	#lnPi<-log(exp(-(DIST^2)/(Vi))/Vi)-log(pi)-log(2) #unsure why eqn4 is squared, this gives incorrect lh; formula below is correct
	lnPi<-log(exp(-(DIST^2)/(2*Vi)))-0.5*log(2*pi*Vi)
	if(is.infinite(-sum(lnPi))){
		return(10000)
		} else{
		return(-sum(lnPi))
		}
}

logLH_BMlin_sis<-function(DIST,TIME,GRAD,pars){
	Vi<-BM_lin_variance_sis(times=TIME, gradient=GRAD, pars=c(b=exp(pars[1]),s0=0,sb=pars[3]))$variance  ##first, calculate Vi for given pars and times using numerical integration
	#lnPi<-log(exp(-(DIST^2)/(Vi))/Vi)-log(pi)-log(2) #unsure why eqn4 is squared, this gives incorrect lh; formula below is correct
	lnPi<-log(exp(-(DIST^2)/(2*Vi)))-0.5*log(2*pi*Vi)
	if(is.infinite(-sum(lnPi))){
		return(10000)
		} else{
		return(-sum(lnPi))
		}
}
