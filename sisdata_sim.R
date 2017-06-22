#simulates data under the MC model, setting parameter values and a vector of times indicating the TMRCA for each species pair


data_sim<-function(times,pars=c(sig2,S)){
	res.mat<-matrix(nrow=length(times),ncol=3)
	colnames(res.mat)<-c("sp1.sim.value","sp2.sim.value","diff")
	for(i in 1:length(times)){
		 hold<-MCsim_sister(times[i],pars=pars)
		 res.mat[i,]<-c(hold[[1]],hold[[2]],(hold[[1]]-hold[[2]]))
	}
	return(data.frame(times,res.mat))
	}
