CREATE_INPUT <-
function(name,type,distr=NULL,param,monoton="dunno",quser=NULL,ruser=NULL,possi_user=NULL){

X=list(
name=name, #variable name
type=type, #possibilistic: "possi" or probabilistic: "proba" or imprecise probability: "impr proba" or fixed:"fixed"
distr=distr, #type of possibilistic or probabilistic variables used (see SAMP_PROBA or SAMP_POSSI for the different choices
param=param, #for variables of types "possi" "proba" or "fixed, fixed parameters needed for the distribution in a vector. for variables of type "impr proba", the index of the input vector corresponding to the variables to be implemented as subvariables
monoton=monoton #variation of the model with respect to the input parameters. increasing: "incr", decreasing: "decr", not monotonic or not known: "dunno". Useless for subvariables of imprecise probabilistic variables
)

if (is.null(possi_user)==FALSE){
	X$possi_user=possi_user
}
	
if (is.null(quser)==FALSE & is.null(ruser)==FALSE){
	if (type=="proba"|type=="impr proba"){
		X$qfun=quser
		X$rfun=ruser
	} else{
	stop("Type should be proba or impr proba")
	}
}

### warnings
if (type != "fixed"){

if (distr=="trapeze" & length(param)!=4){
	warning("Number of parameters inconsistent with the type possibility and a trapezedistribution")
}
if (type=="possi" & distr=="triangle" & length(param)!=3){
	warning("Number of parameters inconsistent with the type possibility and a triangle distribution")
}
if (distr=="interval" & length(param)!=2){
	warning("Number of parameters inconsistent with the type possibility and a interval")
}
if (distr=="normal" & length(param)!=2){
	warning("Number of parameters inconsistent with a normal probability distribution")
}

if (distr=="lognormal" & length(param)!=2){
	warning("Number of parameters inconsistent with a normal probability distribution")
}

if (type=="proba" & distr=="triangle" & length(param)!=3){
	warning("Number of parameters inconsistent with a triangle probability distribution")
}

if (distr=="beta" & length(param)!=2){
	warning("Number of parameters inconsistent with a triangle probability distribution")
}

if (distr=="gumbel" & length(param)!=2){
	warning("Number of parameters inconsistent with a triangle probability distribution")
}

}

if (type=="fixed" & length(param)!=1){
	warning("Number of parameters inconsistent with a fixed variable")
}


return(X)
}
