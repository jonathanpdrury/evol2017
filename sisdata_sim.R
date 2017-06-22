data_sim<-function(d.sis,pars=c(sig2,S)){
	res.mat<-matrix(nrow=dim(d.sis)[1],ncol=3)
	colnames(res.mat)<-c("sp1.sim.value","sp2.sim.value","diff^2")
	for(i in 1:dim(d.sis)[1]){
		 hold<-MCsim_sister(d.sis[i,3],pars=pars,plot=FALSE)
		 res.mat[i,]<-c(hold[[1]],hold[[2]],(hold[[1]]-hold[[2]])^2)
	}
	return(cbind(d.sis,res.mat))
	}
