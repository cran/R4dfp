\name{blank.4dfp}
\alias{blank.4dfp}
\alias{blank.333.4dfp}
\alias{blank.111.4dfp}
\title{Create a Blank 4dfp MRI Image.}
\author{Kevin P. Barry <ta0kira@users.berlios.de> with contributions from Avi Z.
Snyder <avi@npg.wustl.edu>}
\description{
Returns a loaded 4dfp MRI image, either loaded or memory-mapped.
}
\usage{
blank.4dfp(file="",dims=c(1,1,1,1),scale=c(1,1,1),mmppix=c(1,-1,-1),
  center=c(0,0,0))
blank.333.4dfp(file="",t=1)
blank.111.4dfp(file="",t=1)
}
\arguments{
  \item{file}{
  File-name.
  }
  \item{dims}{
  Lengths of each of the 4 dimensions.
  }
  \item{scale}{
  Voxel-to-coordinate scale.
  }
  \item{mmppix}{
  No one really knows.
  }
  \item{center}{
  Centerpoint of the image.
  }
  \item{t}{
  Length of \sQuote{time} dimension.
  }
}
\value{
Object of class \sQuote{R4dfp}.
}
\details{
}
\examples{
#create a new image and load it for direct writing
myimage <- blank.333.4dfp("myimage.4dfp.ifh")
myimage <- recycle.4dfp(myimage,direct.write=TRUE)
}
\references{
}
\seealso{
  \code{\link{save.4dfp}}
  \code{\link{copy.4dfp}}
  \code{\link{close.4dfp}}
  \code{\link{recycle.4dfp}}
}
\keyword{utilities}
\keyword{data}
