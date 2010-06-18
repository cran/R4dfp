\name{copy.4dfp}
\alias{copy.4dfp}
\title{Copy a Loaded 4dfp MRI Image.}
\author{Kevin P. Barry <ta0kira@users.berlios.de> with contributions from Avi Z.
Snyder <avi@npg.wustl.edu>}
\description{
Copies a loaded 4dfp MRI image into memory.
}
\usage{
copy.4dfp(object,file="")
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
oldimage <- blank.333.4dfp()
newimage <- copy.4dfp(oldimage,"newimage.4dfp.ifh")
}
\references{
}
\seealso{
  \code{\link{load.4dfp}}
  \code{\link{save.4dfp}}
  \code{\link{close.4dfp}}
  \code{\link{recycle.4dfp}}
}
\keyword{utilities}
\keyword{data}
