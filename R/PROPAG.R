PROPAG <-
function(N,input,FUN,choice_opt="L-BFGS-B",param_opt=NULL,mode="IRS",corr=1.e-2,NL=10){



if (mode=="IRS"){

####################################
### PROPAGATION - IRS
d=length(input)
rr<-lhsDesign(N,d)$design*(1-corr)
Z0<-pbapply(rr,1,PROPAG_fun,N,input,FUN,choice_opt,param_opt)

}

if (mode=="HYBRID"){

####################################
### PROPAGATION - HYBRID
d=length(input)

rr=matrix(,nrow=N*NL, ncol=d)

l=rep(seq(0,1-corr,length=NL),each=N)

allpossi=0
for (i in 1:d){
	if (input[[i]]$type=="possi"|input[[i]]$type=="fixed"){
		rr[,i]=l
		allpossi=allpossi+1
	} else if (input[[i]]$type=="proba"|input[[i]]$type=="impr proba"){
		rr[,i]=runif(N*NL)
		#TEMP=runif(N)
		#rr[,i]=rep(TEMP,NL)
	}
}
if (allpossi==d){
	rr=matrix(rep(seq(0,1-corr,length=N*NL),d),ncol=d)
}
Z0<-pbapply(rr,1,PROPAG_fun,N,input,FUN,choice_opt,param_opt)

}

return(Z0)

}
