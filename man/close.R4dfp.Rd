\name{close.R4dfp}
\alias{close.R4dfp}
\title{Close a 4dfp MRI Image.}
\author{Kevin P. Barry <ta0kira@users.berlios.de> with contributions from Avi Z.
Snyder <avi@npg.wustl.edu>}
\description{
Closes a loaded 4dfp MRI image.
}
\usage{
close.R4dfp(con,...)
}
\arguments{
  \item{con}{
  Object of class \sQuote{R4dfp}.
  }
  \item{...}{
  Optionally, specify \code{save=TRUE} to save before closing (only if
  \code{direct.read} and \code{direct.write} weren't used.)
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
myimage <- blank.333.4dfp("myimage.4dfp.ifh")
myimage <- close.R4dfp(myimage,save=TRUE)
}
\references{
}
\seealso{
  \code{\link{load.4dfp}}
  \code{\link{save.4dfp}}
  \code{\link{copy.4dfp}}
  \code{\link{recycle.4dfp}}
}
\keyword{utilities}
\keyword{data}
