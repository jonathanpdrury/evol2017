#simulates a trait value for each of two sister taxa under the MC model
    	
    MCsim_sister<-function(time,pars,seglen=0.001){
    
    	sig2<-pars[1]
    	sterm<-pars[2]

    	segmentSize=ceiling(time/seglen)
        traitMat <- matrix(nrow = 2, ncol = segmentSize+1)
		traitMat[,1] = 0
		mu = 0

			
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


	return(list(sp1=tail(traitMat[1,],1),sp2=tail(traitMat[2,],1)))
	}