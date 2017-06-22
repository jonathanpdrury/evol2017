##this takes a sorted vector of times and returns the sum of the variance for a set of sister taxa expected under a given set of parameter values
##uses the analytical solution (thanks to Marc Manceau)

MC_variance_sis<-function(times,pars){
	sig2<-pars[1]
	S<-pars[2]
	return(list(variance=sig2*(times + ((exp(-2*times*S)-1)/2))*2))
}

MC_lin_variance_sis<-function(times,gradient,pars){ #pars[1]=sig2,pars[2]=s0(intercept), pars[3]=sb(slope)
	sig2<-pars[1]
	S<-pars[2]+gradient*pars[3]
	return(list(variance=sig2*(times + ((exp(-2*times*S)-1)/2))*2))
}

BM_lin_variance_sis<-function(times,gradient,pars){ #pars[1]=sig2,pars[2]=s0(intercept), pars[3]=sb(slope)
	sig2<-pars[1]+gradient*pars[3]
	S<-0
	return(list(variance=sig2*(times + ((exp(-2*times*S)-1)/2))*2))
}

