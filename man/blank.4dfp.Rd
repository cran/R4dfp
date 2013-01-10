\name{R4dfp.Blank}
\alias{R4dfp.Blank}
\alias{R4dfp.Blank333}
\alias{R4dfp.Blank111}
\title{Create a Blank 4dfp MRI Image.}
\author{Kevin P. Barry <ta0kira@users.berlios.de> with contributions from Avi Z.
Snyder <avi@npg.wustl.edu>}
\description{
Returns a loaded 4dfp MRI image, either loaded or memory-mapped.
}
\usage{
R4dfp.Blank(file="",dims=c(1,1,1,1),scale=c(1,1,1),mmppix=c(1,-1,-1),
  center=c(0,0,0))
R4dfp.Blank333(file="",t=1)
R4dfp.Blank111(file="",t=1)
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
\examples{
#create a new image and load it for direct writing
myimage <- R4dfp.Blank333("myimage.4dfp.ifh")
myimage <- R4dfp.Recycle(myimage,direct.write=TRUE)
}
\seealso{
  \code{\link{R4dfp.Save}}
  \code{\link{R4dfp.Copy}}
  \code{\link{R4dfp.Close}}
  \code{\link{R4dfp.Recycle}}
}
\keyword{utilities}
\keyword{data}
