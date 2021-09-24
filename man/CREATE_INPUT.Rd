\name{CREATE_INPUT}
\alias{CREATE_INPUT}
\title{
Function to define the input variables  (imprecise, random or fixed). 
}
\description{
Function to define the input variables  (imprecise, random or fixed). Five types are accounted for: 
	\itemize{
		\item Probability distribution (either pre-defined (Normal, log-Normal, uniform, triangular, Beta, Gumbel) or user-defined).
		\item Possibility distribution (either trapezoidal or triangular).
		\item Intervals.
		\item Fixed scalar value.
		\item Imprecise probability distributions (Normal, log-Normal, uniform, triangular, Beta, Gumbel) with parameters 
		represented by by intervals, possibility distributions, fixed scalar values, or probability distributions.
	}
}
\usage{
CREATE_INPUT(name, type, distr = NULL, param, 
		monoton = "dunno", quser = NULL, ruser = NULL)
}
\arguments{
  \item{name}{
	String of the name of the input variable.
}
  \item{type}{
	String to specify the type of input variable: 
	\itemize{
	  	\item "proba": probability distribution.
	  	\item "possi": possibility distribution.
		\item "impr proba": imprecise probability distribution
	  	\item "fixed": fixed scalar value.
	}
}
  \item{distr}{
	String to specify the type of distribution: 
	\itemize{
	  	\item If type="proba" or type="impr proba", distr should be: "normal", "lognormal","uniform", "triangle", "beta", "gumbel","user". 
	  	\item If type="possi", distr should be: "trapeze", "triangle", "interval".
	}
}
  \item{param}{
	Vector of parameter values. This depends on the choice of \emph{distr} and \emph{type}.
	\itemize{
	  	\item If type="proba" and distr="normal", param should be a vector of two values. For example c(0,1) corresponds to a normal distribution with mean=0 and standard deviation of 1.
	  	\item If type="proba" and distr="lognormal", param should be a vector of two values. For example c(0,1) corresponds to a lognormal distribution with meanlog=0 and standard deviation of sdlog=1.
		\item If type="proba" and distr="uniform", param should be a vector of two values. For example c(0,1) corresponds to a uniform distribution with support[0,1].
		\item If type="proba" and distr="triangle", param should be a vector of three values. For example c(0,2,1) corresponds to a triangle distribution with apex=1 and support[0,2]. See help from package \emph{triangle}.
		\item If type="proba" and distr="beta", param should be a vector of two values. For example c(1,1) corresponds to a beta distribution with parameters shape1 and shape2 equal to 1 (see help of qbeta in stats package).
		\item If type="proba" and distr="gumbel", param should be a vector of two values. For example c(0,1) corresponds to a gumbel distribution with location=0 and scale parameter=1.
		\item If type="possi" and distr="trapeze", param should be a vector of four values. For example c(0,1,2,3) corresponds to a possibility distribution with core=[1,2] and support=[0,3].
		\item If type="possi" and distr="triangle", param should be a vector of three values. For example c(0,1,2) corresponds to a possibility distribution with core={1} and support=[0,2].
		\item If type="possi" and distr="interval", param should be a vector of two values. For example c(0,1) corresponds to the interval [0,1].
		\item If type="impr proba", 
		param should be a vector of integers; each integer points to the rank of the input, which represents the uncertainty on the corresponding parameter of chosen distribution
	        For example, if type="impr proba" and distr="normal", c(2,3) corresponds to an imprecise normal distribution, whose imprecise mean is input[[2]] and standard deviation is input[[3]].
	}
}
  \item{monoton}{
	String to specify the monotony of the model function regarding the input variable. 
	\itemize{
		\item "decr" for decreasing.
		\item "incr" for increasing.
		\item "dunno" for unknown monotony or known no-monotony.
	}
}
  \item{quser}{
	If distr="user", this string specifies the quantile function of a probability distribution non listed in the pre-defined ones. The vector of \emph{param} should be updated according to this law.
}
  \item{ruser}{
	If distr="user", string to specify the random sampling function of a probability distribution non listed in the pre-defined ones. The vector of \emph{param} should be updated according to this law.
}
}
\value{
	list with the afore-described arguments.
}

\details{
Details of the theory and the example in Dubois & Guyonnet (2011) 
Available at:
\url{https://hal-brgm.archives-ouvertes.fr/file/index/docid/578821/filename/Uncertainties_RA_09_l_dg.pdf}
}

\seealso{
\code{\link{CREATE_DISTR}}
\code{\link{PLOT_INPUT}}
}
\examples{

#################################################
#### EXAMPLE 1 of Dubois & Guyonnet (2011)
#### Probability and Possibility distributions
#################################################

ninput<-5 #Number of input parameters
input<-vector(mode="list", length=ninput) # Initialisation

input[[1]]=CREATE_INPUT(
		name="UER",
		type="possi",
		distr="triangle",
		param=c(2.e-2, 5.7e-2, 1.e-1),
		monoton="incr"
		)
input[[2]]=CREATE_INPUT(
		name="EF",
		type="possi",
		distr="triangle",
		param=c(200,250,350),
		monoton="incr"
		)
input[[3]]=CREATE_INPUT(
		name="I",
		type="possi",
		distr="triangle",
		param=c(1,1.5,2.5),
		monoton="incr"
		)
input[[4]]=CREATE_INPUT(
		name="C",
		type="proba",
		distr="triangle",
		param=c(5e-3,20e-3,10e-3)
		)
input[[5]]=CREATE_INPUT(
		name="ED",
		type="proba",
		distr="triangle",
		param=c(10,50,30)
		)
		
#################################################
#### EXAMPLE 2 of Sch\"obi & Sudret (2016)
#### Imprecise Probability distributions
#################################################

ninput<-6 #Number of input parameters
input<-vector(mode="list", length=ninput) # Initialisation

# Imprecise normal probability 
# whose parameters are described by the 3rd and 5th parameters
input[[1]]=CREATE_INPUT(
		name="A",
		type="impr proba",
		distr="normal",
		param=c(3,5),
		monoton="dunno"
		)

# Imprecise normal probability
# whose parameters are described by the 4th and 6th parameters
input[[2]]=CREATE_INPUT(
		name="B",
		type="impr proba",
		distr="normal",
		param=c(4,6),
		monoton="dunno"
		)

# imprecise paramters of afore-described probability distribution
# mean of input number 1 as an interval
input[[3]]=CREATE_INPUT(
		name="mu_A",
		type="possi",
		distr="interval",
		param=c(-0.5,0.5)
		)

# mean of input number 2 as an interval
input[[4]]=CREATE_INPUT(
		name="mu_B",
		type="possi",
		distr="interval",
		param=c(-0.5,0.5)
		)

# standard deviation of input number 1  as an interval
input[[5]]=CREATE_INPUT(
		name="s_A",
		type="possi",
		distr="interval",
		param=c(0.7,1)
		)

# standard deviation of input number 2  as an interval
input[[6]]=CREATE_INPUT(
		name="s_B",
		type="possi",
		distr="interval",
		param=c(0.7,1)
		)
		
}
