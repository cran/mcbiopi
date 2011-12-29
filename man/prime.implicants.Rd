\name{prime.implicants}
\alias{prime.implicants}
\alias{print.primeImp}
\title{Prime Implicants}
\description{
  Computes the prime implicants of a given truth table.
}
\usage{
  prime.implicants(mat)
}
\arguments{
  \item{mat}{a matrix containing only 0's and 1's. Each column of \code{mat}
     corresponds to a binary variable and each row to a combination of the
     variables for which the logic expression is TRUE.}
}


\value{
  An object of class \code{primeImp} containing a vector \code{vec.primes} comprising the prime implicants 
  and a matrix \code{mat.primes} representing the prime implicant table.
}

\references{
   Schwender, H. (2007).  Minimization of Boolean Expressions Using Matrix Algebra. 
   Technical Report, SFB 475, Department of Statistics, TU Dortmund University.
}

\author{Holger Schwender, \email{holger.schwender@udo.edu}}

\seealso{
   \code{\link{minDNF}}
}

\keyword{logic}
\keyword{print}
