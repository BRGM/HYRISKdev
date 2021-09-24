\name{CREATE_POSSI_USER}
\alias{CREATE_POSSI_USER}
\title{
Function to create a user-defined possibility
}
\description{
Function to create a user-defined possibility by following two options from: (1) a set of confidence intervals; (2) a mean and standard deviation value using the inquality of Bienayme-Chebychev.
}
\usage{
CREATE_POSSI_USER(listX,listL,type="confidence",DISCR=1000)
}
\arguments{
  \item{listX}{
	  \itemize{
		\item If type="confidence", listX corresponds to the list of endpoints of the considered intervals. For instance, c(0,1,2,3) corresponds to intervals [0,1], [1,2] and [2,3].
		\item If type="bracket", listX corresponds to mean value.
	  }  
  }
  \item{ListY}{
	  \itemize{
		\item If type="confidence", lisYX corresponds to the list of possibility degrees. For instance, c(0.2,1,0.05) corresponds to 3 intervals, the degrees 0.2, 1.0 and 0.05.
		\item If type="bracket", listY corresponds to a vector of two values: the standard deviation and the limit of truncation (expressed in number of standard deviation). For instance, c(0.1,3) corresponds to a standard deviation of 0.1 and a limit of 3 times the standard deviation. 
	  }  
  }
}

\details{
\itemize{
	\item Details on the approach using the confidence interval, see Baudrit et al. 2006.
	\item Details on the approach using the inequality of Bienayme-Chebychev, see Sect. 3.3 of \url{https://www.irit.fr/~Henri.Prade/Papers/DFoulMauP.pdf}
}
}
\examples{
#################################################
### user-defined
### mode: "confidence"
### user information: series of intervals with associated degree of possibilites
#################################################
listX<-c(0.3,0.52,0.98,1.5,2,2.5)## liste des bornes des intervalles de confiance
listL<-c(0.33,1,0.2,0.05,0.05) ## liste des niveaux de possibilité (nbre = nbre_listX-1)
a<-CREATE_POSSI_USER(listX,listL)
plot(a)

#################################################
### mode: "bracket"
### inegalite de Bienayme-Chebychev; voir: sect. 3.3 de https://www.irit.fr/~Henri.Prade/Papers/DFoulMauP.pdf
### user information: mean, standard deviation and limit of truncation
#################################################
listX<-c(0.5) ## mean
listL<-c(0.1,3) ## standard dev. + limit of truncation (in number of standard dev.)
b<-CREATE_POSSI_USER(listX,listL,type="bracket")
plot(b)
}
