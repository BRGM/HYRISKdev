PROBA_INTERVAL <-
function(Z0,threshold){
	#F1<- kde(type_kernel="e", vec_data=Z0[1,],
	#		y=threshold)$Estimated_values
	#F2<- kde(type_kernel="e", vec_data=Z0[2,],
	#		y=threshold)$Estimated_values
	e <- Z0[1,]
	F1 <- ecdf(e)(threshold)
	e <- Z0[2,]
	F2 <- ecdf(e)(threshold)	
	return(list(Plow=min(F1,F2),Pupp=max(F1,F2)))
}
