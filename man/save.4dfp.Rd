\name{R4dfp.Save}
\alias{R4dfp.Save}
\title{Write a 4dfp MRI Image.}
\author{Kevin P. Barry <ta0kira@users.berlios.de> with contributions from Avi Z.
Snyder <avi@npg.wustl.edu>}
\description{
Attempts to write a 4dfp MRI image.
}
\usage{
R4dfp.Save(object)
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
\seealso{
  \code{\link{R4dfp.Load}}
  \code{\link{R4dfp.Copy}}
  \code{\link{R4dfp.Close}}
  \code{\link{R4dfp.Recycle}}
}
\keyword{utilities}
\keyword{data}
