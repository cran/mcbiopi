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

\author{Holger Schwender, \email{holger.schwender@hhu.de}}

\seealso{
   \code{\link{prime.implicants}}
}

\examples{\dontrun{
# Generate the truth table considered in Schwender (2007).

mat <- matrix(c(rep(0, 4), rep(1, 6), 
   rep(0, 6), rep(1, 4),
   0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 
   0, 1, 0, 1, 1, 1, 0, 1, 0, 1), ncol=4)
colnames(mat) <- paste("X", 1:4, sep="")

# Computing the minimal disjunctive normal form.

minDNF(mat)
}}


\keyword{optimize}
\keyword{logic}
\keyword{print}
