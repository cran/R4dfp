\name{R4dfp.Copy}
\alias{R4dfp.Copy}
\title{Copy a Loaded 4dfp MRI Image.}
\author{Kevin P. Barry <ta0kira@users.berlios.de> with contributions from Avi Z.
Snyder <avi@npg.wustl.edu>}
\description{
Copies a loaded 4dfp MRI image into memory.
}
\usage{
R4dfp.Copy(object,file="")
}
\arguments{
  \item{object}{
  Object of class \sQuote{R4dfp}.
  }
  \item{file}{
  An alternate file-name to give the copied file.
  }
}
\value{
Object of class \sQuote{R4dfp}.
}
\details{
Directly copies everything except for the name of the file. Always use this
to copy \sQuote{R4dfp} instead of \code{<-}!
}
\examples{
#copy another loaded image
oldimage <- R4dfp.Blank333()
newimage <- R4dfp.Copy(oldimage,"newimage.4dfp.ifh")
}
\seealso{
  \code{\link{R4dfp.Load}}
  \code{\link{R4dfp.Save}}
  \code{\link{R4dfp.Close}}
  \code{\link{R4dfp.Recycle}}
}
\keyword{utilities}
\keyword{data}
