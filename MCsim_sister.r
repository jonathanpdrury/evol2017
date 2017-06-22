    	
    	
    MCsim_sister<-function(time,pars,plot=FALSE,seglen=0.001){
    
    	sig2<-pars[1]
    	sterm<-pars[2]

    	segmentSize=ceiling(time/seglen)
        traitMat <- matrix(nrow = 2, ncol = segmentSize+1)
		traitMat[,1] = 0
		mu = 0
		tempInd<- 1:2 # hack to have fast selection of not k, seemed to be faster than a call to which()

			
    	for(k in 1:segmentSize){
	          	for(j in 1:2){
	            	mu.value<-tail(mu,n=1) 
	            	traitMat[j,k+1]<-traitMat[j,k] + sterm*(mu.value-traitMat[j,k])*seglen +rnorm(1,0,sqrt(sig2*seglen))
					if(k==segmentSize && time%%seglen!=0){
						seglenT= time%%seglen
	            		traitMat[j,k+1]<-traitMat[j,k] + sterm*(mu.value-traitMat[j,k])*seglenT +rnorm(1,0,sqrt(sig2*seglenT))
						}
				}
    		mu=c(mu, mean(traitMat[,k+1]))
    	}

 	if(plot==TRUE){
  	  	M=seq(0,time,length=(segmentSize+1))
    	dd<-cbind(M,t(traitMat))
    	t.plot<-plot(M,1:length(M),col="white", ylim=c(range(sapply(traitMat,range))), xlab="time", ylab="Value")
		lines(dd[,1],dd[,2])
		lines(dd[,1],dd[,3])
  	}


	return(list(sp1=tail(traitMat[1,],1),sp2=tail(traitMat[2,],1)))
	}