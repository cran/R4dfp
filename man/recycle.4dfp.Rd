\name{R4dfp.Recycle}
\alias{R4dfp.Recycle}
\title{Reload a 4dfp MRI Image.}
\author{Kevin P. Barry <ta0kira@users.berlios.de> with contributions from Avi Z.
Snyder <avi@npg.wustl.edu>}
\description{
Reloads a loaded 4dfp MRI image with different options.
}
\usage{
R4dfp.Recycle(object,save=TRUE,direct.read=FALSE,direct.write=FALSE)
}
\arguments{
  \item{object}{
  Object of class \sQuote{R4dfp}.
  }
  \item{save}{
  Save the loaded image before doing anything else if \code{direct.read} and
  \code{direct.write} weren't used for the old image.
  }
  \item{direct.read}{
  Pass \code{direct.read} to \code{\link{R4dfp.Load}} when reopening.
  }
  \item{direct.write}{
  Pass \code{direct.write} to \code{\link{R4dfp.Load}} when reopening.
  }
}
\value{
Object of class \sQuote{R4dfp}.
}
\details{
Closes the specified 4dfp image and reopens it with the specified options. This
will invalidate the provided variable; therefore, you must assign the return
back to the variable you pass as \code{object}.

This function is useful for making a copy of the data for an image loaded with
\code{direct.read}, and also for creating a new image and subsequently gaining
direct access to the image file.
}
\examples{
#create an image and reload it with direct writing
myimage <- R4dfp.Blank333("newimage.4dfp.ifh")
myimage <- R4dfp.Recycle(myimage,direct.write=TRUE)
}
\seealso{
  \code{\link{R4dfp.Load}}
  \code{\link{R4dfp.Save}}
  \code{\link{R4dfp.Copy}}
  \code{\link{R4dfp.Close}}
}
\keyword{utilities}
\keyword{data}
