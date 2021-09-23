PLOT_CDF <-
function(Z0,new=TRUE,color1=1,color2=2,...){
if(new==TRUE){
#dev.new()
plot(ecdf(Z0[1,]),xlim=c(min(knots(ecdf(Z0[1,]))),max(knots(ecdf(Z0[2,])))),col=color1,...)
lines(ecdf(Z0[2,]),col=color2,...)
}
if(new==FALSE){
lines(ecdf(Z0[1,]),col=color1,...)
lines(ecdf(Z0[2,]),col=color2,...)
}
}
