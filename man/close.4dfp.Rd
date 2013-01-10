\name{R4dfp.Close}
\alias{R4dfp.Close}
\title{Close a 4dfp MRI Image.}
\author{Kevin P. Barry <ta0kira@users.berlios.de> with contributions from Avi Z.
Snyder <avi@npg.wustl.edu>}
\description{
Closes a loaded 4dfp MRI image.
}
\usage{
R4dfp.Close(object,save=FALSE)
}
\arguments{
  \item{object}{ Object of class \sQuote{R4dfp}.
  }
  \item{save}{Optionally, specify \code{save=TRUE} to save before closing (only if \code{direct.read} and \code{direct.write} weren't used.)
  }
}
\value{
Object of class \sQuote{R4dfp}.
}
\details{
Closes the specified 4dfp image and deallocates its resources. This
will invalidate the \sQuote{R4dfp} provided.
}
\examples{
#create a new image and close it
myimage <- R4dfp.Blank333("myimage.4dfp.ifh")
myimage <- R4dfp.Close(myimage,save=TRUE)
}

\seealso{
  \code{\link{R4dfp.Load}}
  \code{\link{R4dfp.Save}}
  \code{\link{R4dfp.Copy}}
  \code{\link{R4dfp.Recycle}}
}
\keyword{utilities}
\keyword{data}
