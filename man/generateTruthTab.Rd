\name{generateTruthTab}
\alias{generateTruthTab}
\alias{getPImps}

\title{Truth Table for a Logic Tree}
\description{
  Generates the truth table or the prime implicants, respectively, for a logic tree built in a logic regression,
}
\usage{
generateTruthTab(ltree)

getPImps(ltree, type)
}

\arguments{
  \item{ltree}{an object of class \code{logregtree}.}
  \item{type}{the type of the logic regression model that has been fitted.} 
}

  
\author{Holger Schwender, \email{holger.schwender@udo.edu}}


\seealso{
   \code{\link{prime.implicants}}
}

\keyword{utilities}
\keyword{logic}
