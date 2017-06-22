#fits EvoRAG-like models to empirical dataset. For details on DIST, TIME, and GRAD, see ?model.test.sisters for EvoRAG package
#Models included are BM_null & BM_linear (as in EvoRAG), MC_null (one S value estimated for entire dataset), MC_linear (MC with intercept and slope for S value), MC_linear_zerointercept (MC model estimated for non-zero gradient values, BM model for gradient values = 0)

model.test.sisters_MC<-function(DIST, TIME, GRAD, model = c("BM_null", "BM_linear", "MC_null", 
	 "MC_linear", "MC_linear_zerointercept"))
	 
	{
	 if(model=="BM_null"){
	   	op <- getOption("show.error.messages")
		options(show.error.messages=FALSE)
		fitsis<-try(suppressWarnings(optim(par=c(log(0.05)),logLH_BMsis,DIST=DIST,TIME=TIME)))
		if(class(fitsis)!="try-error"){
			return(list(lnL=-fitsis$value,AICc=(2*1-2*(-fitsis$value))+(4/(length(TIME)-2)), n=1, sig2=exp(fitsis$par[1]), convergence=fitsis$convergence))
		}
  		options(show.error.messages=op)
	 }
	 if(model=="BM_linear"){
	   	op <- getOption("show.error.messages")
		options(show.error.messages=FALSE)
		fitsis<-try(suppressWarnings(optim(par=c(log(0.05),0,0.01),logLH_BMlin_sis,DIST=DIST,TIME=TIME,GRAD=GRAD)))
		if(class(fitsis)!="try-error"){
			return(list(lnL=-fitsis$value,AICc=(2*2-2*(-fitsis$value))+(12/(length(TIME)-3)),n=2,  sig2_0=exp(fitsis$par[1]), sig2_b=fitsis$par[3], convergence=fitsis$convergence))
		}
  		options(show.error.messages=op)
	 }
	 if(model=="MC_null"){
	   	op <- getOption("show.error.messages")
		options(show.error.messages=FALSE)
		fitsis<-try(suppressWarnings(optim(par=c(log(0.05),-.10),logLH_MCsis,DIST=DIST,TIME=TIME)))
		if(class(fitsis)!="try-error"){
			return(list(lnL=-fitsis$value,AICc=(2*2-2*(-fitsis$value))+(12/(length(TIME)-3)),n=2,  sig2=exp(fitsis$par[1]), S=-abs(fitsis$par[2]), convergence=fitsis$convergence))
		}
  		options(show.error.messages=op)
	 }
	 if(model=="MC_linear"){
	   	op <- getOption("show.error.messages")
		options(show.error.messages=FALSE)
		fitsis<-try(suppressWarnings(optim(par=c(log(0.05),-.10,-.10),logLH_MClin_sis,DIST=DIST,TIME=TIME,GRAD=GRAD)))
		if(class(fitsis)!="try-error"){
			return(list(lnL=-fitsis$value,AICc=(2*3-2*(-fitsis$value))+(18/(length(TIME)-4)), n=3, sig2=exp(fitsis$par[1]), S_0=-abs(fitsis$par[2]), S_b=-abs(fitsis$par[3]), convergence=fitsis$convergence))
		}
  		options(show.error.messages=op)
	 }
	 if(model=="MC_linear_zerointercept"){ 
	   	op <- getOption("show.error.messages")
		options(show.error.messages=FALSE)
		fitsis<-try(suppressWarnings(optim(par=c(log(0.05),-.10,-.10),logLH_MClin_sis_zerointercept,DIST=DIST,TIME=TIME,GRAD=GRAD)))
		if(class(fitsis)!="try-error"){
			return(list(lnL=-fitsis$value,AICc=(2*3-2*(-fitsis$value))+(18/(length(TIME)-4)), n=3, sig2=exp(fitsis$par[1]), S_0="forced to 0", S_b=-abs(fitsis$par[3]),convergence=fitsis$convergence))
		}
  		options(show.error.messages=op)
	 }
	 
	}
