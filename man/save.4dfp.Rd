\name{save.4dfp}
\alias{save.4dfp}
\title{Write a 4dfp MRI Image.}
\author{Kevin P. Barry <ta0kira@users.berlios.de> with contributions from Avi Z.
Snyder <avi@npg.wustl.edu>}
\description{
Attempts to write a 4dfp MRI image.
}
\usage{
save.4dfp(object)
}
\arguments{
  \item{object}{
  Object of class \sQuote{R4dfp}.
  }
}
\value{
Object of class \sQuote{R4dfp}.
}
\details{
Attempts to save the specified 4dfp image using the header name in
\code{object$file}. Images loaded using \code{direct.read} or
\code{direct.write} can't be saved.
}
\references{
}
\seealso{
  \code{\link{load.4dfp}}
  \code{\link{copy.4dfp}}
  \code{\link{close.4dfp}}
  \code{\link{recycle.4dfp}}
}
\keyword{utilities}
\keyword{data}
