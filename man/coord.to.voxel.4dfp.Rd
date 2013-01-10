\name{R4dfp.CoordToVoxel}
\alias{R4dfp.CoordToVoxel}
\title{Translate a Coordinate to a Voxel.}
\author{Kevin P. Barry <ta0kira@users.berlios.de> with contributions from Avi Z.
Snyder <avi@npg.wustl.edu>}
\description{
Translate a coordinate to a voxel for a specific 4dfp MRI image.
}
\usage{
R4dfp.CoordToVoxel(object,coord)
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
myimage <- R4dfp.Blank333()
voxel <- R4dfp.CoordToVoxel(myimage,c(0,0,0))
}
\seealso{
  \code{\link{R4dfp.VoxelToCoord}}
  \code{\link{R4dfp.Load}}
  \code{\link{R4dfp.Save}}
  \code{\link{R4dfp.Close}}
  \code{\link{R4dfp.Recycle}}
}
\keyword{utilities}
\keyword{data}
