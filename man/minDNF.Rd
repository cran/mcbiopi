\name{minDNF}
\alias{minDNF}
\alias{print.minDNF}
\title{Minimum Disjunctive Normal Form}
\description{
  Computes the minimal disjuntive normal form for a given truth table.
}
\usage{
  minDNF(mat)
}
\arguments{
  \item{mat}{a matrix containing only 0's and 1's. Each column of \code{mat}
     corresponds to a binary variable and each row to a combination of the
     variables for which the logic expression is \code{TRUE}.}
}


\value{
  An object of class \code{minDNF} containing a vector comprising a minimized set of prime implicants. If more than
  one solution exist, then a list is returned containing all solutions.
}

\references{
   Schwender, H. (2007).  Minimization of Boolean Expressions Using Matrix Algebra. 
   Technical Report, SFB 475, Department of Statistics, TU Dortmund University.
}

\author{Holger Schwender, \email{holger.schwender@udo.edu}}

\seealso{
   \code{\link{prime.implicants}}
}

\keyword{optimize}
\keyword{logic}
\keyword{print}
