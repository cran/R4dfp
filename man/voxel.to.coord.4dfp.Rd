\name{voxel.to.coord.4dfp}
\alias{voxel.to.coord.4dfp}
\title{Translate a Voxel to a Coordinate.}
\author{Kevin P. Barry <ta0kira@users.berlios.de> with contributions from Avi Z.
Snyder <avi@npg.wustl.edu>}
\description{
Translate a voxel to a coordinate for a specific 4dfp MRI image.
}
\usage{
voxel.to.coord.4dfp(object,voxel)
}
\arguments{
  \item{object}{
  Object of class \sQuote{R4dfp}.
  }
  \item{voxel}{
  An Nx3 matrix of voxels to translate into coordinates.
  }
}
\value{
Nx3 matrix of coordinates.
}
\details{
Finds the nearest coordinate to the given voxels. Image limits aren't checked.
}
\examples{
#get the coordinate of the first voxel
myimage <- blank.333.4dfp()
coord <- voxel.to.coord.4dfp(myimage,c(1,1,1))
}
\references{
}
\seealso{
  \code{\link{coord.to.voxel.4dfp}}
  \code{\link{load.4dfp}}
  \code{\link{save.4dfp}}
  \code{\link{close.4dfp}}
  \code{\link{recycle.4dfp}}
}
\keyword{utilities}
\keyword{data}
