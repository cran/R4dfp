\name{R4dfp.VoxelToCoord}
\alias{R4dfp.VoxelToCoord}
\title{Translate a Voxel to a Coordinate.}
\author{Kevin P. Barry <ta0kira@users.berlios.de> with contributions from Avi Z.
Snyder <avi@npg.wustl.edu>}
\description{
Translate a voxel to a coordinate for a specific 4dfp MRI image.
}
\usage{
R4dfp.VoxelToCoord(object,voxel)
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
myimage <- R4dfp.Blank333()
coord <- R4dfp.VoxelToCoord(myimage,c(1,1,1))
}
\seealso{
  \code{\link{R4dfp.CoordToVoxel}}
  \code{\link{R4dfp.Load}}
  \code{\link{R4dfp.Save}}
  \code{\link{R4dfp.Close}}
  \code{\link{R4dfp.Recycle}}
}
\keyword{utilities}
\keyword{data}
