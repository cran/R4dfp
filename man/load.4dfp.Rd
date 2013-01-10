\name{R4dfp.Load}
\alias{R4dfp.Load}
\alias{R4dfp.Print}
\alias{is.4dfp}
\title{Read a 4dfp MRI Image.}
\author{Kevin P. Barry <ta0kira@users.berlios.de> with contributions from Avi Z.
Snyder <avi@npg.wustl.edu>}
\description{
Returns a loaded 4dfp MRI image, either loaded or memory-mapped.
}
\usage{
R4dfp.Load(file,direct.read=FALSE,direct.write=FALSE)
R4dfp.Print(x)
is.4dfp(unknown)
}
\arguments{
  \item{file}{
  Path name of the image to load.
  }
  \item{direct.read}{
  Flag indicating if the .img should be loaded with \sQuote{mmap} for reading.
  }
  \item{direct.write}{
  Flag indicating if the .img should be loaded with \sQuote{mmap} for writing.
  This implies \code{direct.read}, also.
  }
  \item{x}{
  Object of class \sQuote{R4dfp}.
  }
  \item{unknown}{
  An object to check for class \sQuote{R4dfp} or a string to check for 4dfp
  file-name format.
  }
}
\value{
Object of class \sQuote{R4dfp}.
}
\details{
The header/image file pair corresponding to the file-name given will be loaded.
The object returned should not be copied directly with \code{<-} after the
initial assignment; use \code{\link{R4dfp.Copy}} instead!

If \code{direct.read} or \code{direct.write} are specified, the header
information cannot be edited. See \code{\link{R4dfp.Recycle}} for info about
switching to a fully-editable copy. As these options imply, all changes made to
the image will be written directly to the file, and all reads will be read from
the file as it exists at the time of the read.

Voxel access is via the \code{[...]} operator:
  \enumerate{
    \item To access a single voxel, specify 4 scalars, e.g.
    \code{myimage[1,1,1,1]}.
    \item To access the time-series of a single voxel, leave out the time, e.g.
    \code{myimage[1,1,1]}.
    \item To access a range of voxels (i.e. a rectangle,) use sequences or
    vectors for each applicable position, e.g. \code{myimage[1:10,2:5,1]} or
    \code{myimage[c(1,2,4),1,1]}.
    \item To access a list of specific voxels, use a single Nx4 matrix.
    \item To access the time-series of a list of specific voxels, use a single
    Nx3 matrix. Optionally, provide a \code{t} argument to specify frames.
    \item Use a 3- or 4-D logical matrix to choose specific voxels, e.g.
    \code{myimage[myimage[,,,]==NaN] <- 0}. Optionally, provide a \code{t} argument
    to specify specific frames with a 3-D matrix.
  }
}
\examples{
myimage <- R4dfp.Blank333("myimage.4dfp.ifh")
R4dfp.Save(myimage)

#3 different ways to load an image
myimage <- R4dfp.Load("myimage.4dfp.ifh")
myimage <- R4dfp.Load("myimage.4dfp.ifh",direct.read=TRUE)
myimage <- R4dfp.Load("myimage.4dfp.ifh",direct.write=TRUE)

#retrieve a 3x3x3 cube of voxels
myimage[1:3,1:3,1:3,1]

#set a list of voxels to 0 for all frames
voxels <- NULL
voxels <- rbind(voxels,c(1,1,1))
voxels <- rbind(voxels,c(1,1,2))
voxels <- rbind(voxels,c(1,2,1))
voxels <- rbind(voxels,c(1,2,2))
myimage[voxels] <- 0

#set all voxels to something random
myimage[,,,] <- array(rnorm(prod(myimage$dims)),myimage$dims)

#set everything less than 0 to 0
myimage[myimage[,,,]<0] <- 0
}
\seealso{
  \code{\link{R4dfp.Save}}
  \code{\link{R4dfp.Copy}}
  \code{\link{R4dfp.Close}}
  \code{\link{R4dfp.Recycle}}
}
\keyword{utilities}
\keyword{data}
