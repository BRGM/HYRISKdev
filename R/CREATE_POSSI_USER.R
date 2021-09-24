CREATE_POSSI_USER<-function(listX,listL,type="confidence",DISCR=1000){

sets_options("universe",NULL)

if (type=="confidence"){

### mode: "confidence"
### user information: series of intervals with associated degree of possibilites

##listX= liste des bornes des intervalles de confiance
##listL= liste des niveaux de possibilité (nbre = nbre_listX-1)

X <- seq(from = range(listX)[1]*0.5,to = range(listX)[2]*1.5, by = (range(listX)[2]-range(listX)[1])/DISCR)
ms<-NULL

for (i in 1:length(X)){
	for (k in 1:(length(listX)-1)){
		if (X[i]>=listX[k] & X[i]<listX[k+1]){ms[i]<-listL[k]}
	}

	if (X[i]<listX[1]){ms[i]<-2.220446e-16}
	if (X[i]>listX[length(listX)]){ms[i]<-2.220446e-16}


}
a<- gset(support = X, memberships = ms)

}

if (type=="bracket"){

### mode: "bracket"
### inegalite de Bienayme´-Chebychev; voir: sect. 3.3 de https://www.irit.fr/~Henri.Prade/Papers/DFoulMauP.pdf
### user information: mean, standard deviation and limit of truncation
##listX= moyenne
##listL= standard deviation + limite (en nombre de standard dev.)

supX=listX[1]+(listL[2]+1)*listL[1]
infX=listX[1]-(listL[2]+1)*listL[1]
X <- seq(from = infX,to = supX, by = (supX-infX)/DISCR)

ms<-NULL
mu<-listX
sig<-listL[1]
for (i in 1:length(X)){
	for (k in seq(1,listL[2],by=0.01)){
		if (X[i] >= mu-k*sig & X[i] <= mu+k*sig){ms[i]<-1/k^2;break;}
	}
}
ms[which(X<mu-listL[2]*sig)]<-2.220446e-16
ms[which(X>mu+listL[2]*sig)]<-2.220446e-16

a<- gset(support = X, memberships = ms)


}

return(a)


}