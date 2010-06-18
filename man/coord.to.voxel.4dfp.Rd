\name{coord.to.voxel.4dfp}
\alias{coord.to.voxel.4dfp}
\title{Translate a Coordinate to a Voxel.}
\author{Kevin P. Barry <ta0kira@users.berlios.de> with contributions from Avi Z.
Snyder <avi@npg.wustl.edu>}
\description{
Translate a coordinate to a voxel for a specific 4dfp MRI image.
}
\usage{
coord.to.voxel.4dfp(object,coord)
}
\arguments{
  \item{object}{
  Object of class \sQuote{R4dfp}.
  }
  \item{coord}{
  An Nx3 matrix of coordinates to translate into voxels.
  }
}
\value{
Nx3 matrix of voxels.
}
\details{
Finds the nearest voxel to the given coordinates. Image limits aren't checked.
}
\examples{
#get the nearest voxel to the origin
myimage <- blank.333.4dfp()
voxel <- coord.to.voxel.4dfp(myimage,c(0,0,0))
}
\references{
}
\seealso{
  \code{\link{voxel.to.coord.4dfp}}
  \code{\link{load.4dfp}}
  \code{\link{save.4dfp}}
  \code{\link{close.4dfp}}
  \code{\link{recycle.4dfp}}
}
\keyword{utilities}
\keyword{data}
