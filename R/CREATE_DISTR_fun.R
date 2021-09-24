CREATE_DISTR_fun <-
function(X,DISCR){
#library for the possibility distribution
#library(sets)
#library for the probability distribution
#library(triangle)
#library(stats)
#library(reliaR)

if (X$type=="possi"){
	if (X$distr=="triangle"){
		sets_options("universe", seq(from = X$param[1],to = X$param[length(X$param)], by = (X$param[length(X$param)]-X$param[1])/DISCR))
		X$fuzzy<-fuzzy_triangular_gset(corners = c(X$param[1], X$param[2], X$param[3]), height = c(1))
	} else if (X$distr=="trapeze"){
		sets_options("universe", seq(from = X$param[1],to = X$param[length(X$param)], by = (X$param[length(X$param)]-X$param[1])/DISCR))
		X$fuzzy<-fuzzy_trapezoid_gset(corners = c(X$param[1], X$param[2], X$param[3],X$param[4]), height = c(1))
	} else if (X$distr=="user"){
		sets_options("universe", seq(from = range(X$possi_user)[1],to =range(X$possi_user)[2], by = (range(X$possi_user)[2]-range(X$possi_user)[1])/DISCR))
		X$fuzzy<-X$possi_user
	}
} else if (X$type=="proba" | X$type=="impr proba"){
	if (X$distr=="normal"){
		X$qfun=qnorm
		X$rfun=rnorm
	} else if (X$distr=="lognormal"){
		X$qfun=qlnorm
		X$rfun=rlnorm
	} else if (X$distr=="triangle"){
		X$qfun=qtriangle
		X$rfun=rtriangle
	} else if (X$distr=="uniform"){
		X$qfun=qunif
		X$rfun=runif
	} else if (X$distr=="gumbel"){
		X$qfun=qgumbel
		X$rfun=rgumbel
	} else if (X$distr=="beta"){
		X$qfun=qbeta
		X$rfun=rbeta
	} 
}
return(X)
}
