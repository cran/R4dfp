load.4dfp <- function(file,direct.read=FALSE,direct.write=FALSE)
{
  direct.read <- direct.read||direct.write
  new.image <- .Call("load_4dfp",as.character(file),as.integer(direct.read),as.integer(direct.write),PACKAGE="R4dfp")
  names(new.image) <- c("internal","file","dims","scale","mmppix","center")
  class(new.image) <- "R4dfp"
  attr(new.image,'direct.read')  <- direct.read
  attr(new.image,'direct.write') <- direct.write
  return(new.image)
}


save.4dfp <- function(object)
{
  if (!inherits(object,"R4dfp"))
    stop("not a 4dfp image object")

  .Call("save_4dfp",object,PACKAGE="R4dfp")
}


recycle.4dfp <- function(object,save=TRUE,direct.read=FALSE,direct.write=FALSE)
{
  if (!inherits(object,"R4dfp"))
    stop("not a 4dfp image object")

  file <- object$file
  close.R4dfp(object,save=save)
  return(load.4dfp(file,direct.read=direct.read,direct.write=direct.write))
}


copy.4dfp <- function(object,file="")
{
  if (!inherits(object,"R4dfp"))
    stop("not a 4dfp image object")

  new.image <- .Call("blank_4dfp",PACKAGE="R4dfp")
  names(new.image) <- c("internal","file","dims","scale","mmppix","center")
  class(new.image) <- "R4dfp"
  new.image$file   <- file
  new.image$scale  <- object$scale
  new.image$mmppix <- object$mmppix
  new.image$center <- object$center
  new.image$dims   <- object$dims
  new.image[,,,]   <- object[,,,]
  return(new.image)
}


blank.4dfp <- function(file="",dims=c(1,1,1,1),scale=c(1,1,1),mmppix=c(1,-1,-1),center=c(0,0,0))
{
  new.image <- .Call("blank_4dfp",PACKAGE="R4dfp")
  names(new.image) <- c("internal","file","dims","scale","mmppix","center")
  class(new.image) <- "R4dfp"
  attr(new.image,'direct.read')  <- FALSE
  attr(new.image,'direct.write') <- FALSE
  new.image$file   <- file
  new.image$scale  <- scale
  new.image$mmppix <- mmppix
  new.image$center <- center
  new.image$dims   <- dims
  new.image[,,,]   <- 0
  return(new.image)
}


blank.333.4dfp <- function(file="",t=1)
{
  return(blank.4dfp(file=file,dims=c(48,64,48,t),scale=c(3,3,3),mmppix=c(3,-3,-3),center=c(73.5,-87,-84)))
}


blank.111.4dfp <- function(file="",t=1)
{
  return(blank.4dfp(file=file,dims=c(176,208,176,t),scale=c(1,1,1),mmppix=c(1,-1,-1),center=c(89,-85,-101)))
}


"[.R4dfp" <- function(object,X=1:object$dims[1],Y=1:object$dims[2],Z=1:object$dims[3],t=1:object$dims[4])
{
  if (is.logical(X))
    X <- which(X)
  if (is.logical(Y))
    Y <- which(Y)
  if (is.logical(Z))
    Z <- which(Z)
  if (is.logical(t))
    t <- which(t)

  image.data <- .Call("read_voxels_4dfp",object,X-1,Y-1,Z-1,t-1,PACKAGE="R4dfp")

  if (length(image.data))
  {
    if (!is.null(ncol(X))&&ncol(X)==3)
      return(array(zapsmall(image.data),c(nrow(X),length(t)))) else
    if (!is.null(ncol(X))&&ncol(X)==4)
      return(array(zapsmall(image.data),c(nrow(X),1))) else
    return(array(zapsmall(image.data),c(length(X),length(Y),length(Z),length(t))))
  } else
    return(image.data)
}


"[<-.R4dfp" <- function(object,X=1:object$dims[1],Y=1:object$dims[2],Z=1:object$dims[3],t=1:object$dims[4],value)
{
  if (is.logical(X))
    X <- which(X)
  if (is.logical(Y))
    Y <- which(Y)
  if (is.logical(Z))
    Z <- which(Z)
  if (is.logical(t))
    t <- which(t)

  if (length(value)!=1)
  {
    if (!is.null(ncol(X))&&ncol(X)==3)
    {
      if (length(value)!=nrow(X)*length(t))
        stop("number of assignments doesn\'t match number of elements")
    } else
    if (!is.null(ncol(X))&&ncol(X)==4)
    {
      if (length(value)!=nrow(X))
        stop("number of assignments doesn\'t match number of elements")
    } else
    {
      if (length(value)!=1&&!prod(dim(value)==c(length(X),length(Y),length(Z),length(t))))
        stop("number of assignments doesn\'t match number of elements")
    }
  }

  .Call("write_voxels_4dfp",object,X-1,Y-1,Z-1,t-1,as.vector(as.real(value)),PACKAGE="R4dfp")
}


"[[.R4dfp" <- function(object,symbol)
{
  view.copy <- unclass(object)
  switch(symbol,
    file=return(view.copy[[symbol]]),
    dims=return(view.copy[[symbol]]),
    scale=return(view.copy[[symbol]]),
    mmppix=return(view.copy[[symbol]]),
    center=return(view.copy[[symbol]]),
    direct.read=attr(object,'direct.read'),
    direct.write=attr(object,'direct.write'),
    stop("invalid image attribute"))
}


"[[<-.R4dfp" <- function(object,symbol,value)
{
  return(.Call("attribute_change_4dfp",object,symbol,value,PACKAGE="R4dfp"))
}


"$.R4dfp" <- function(object,symbol)
{
  return(object[[symbol]])
}


"$<-.R4dfp" <- function(object,symbol,value)
{
  object[[symbol]] <- value
  return(object)
}


is.4dfp <- function(unknown)
{
  if (class(unknown)=="R4dfp")
    return(TRUE) else
  if (is.character(unknown))
    return(length(grep('\\.4dfp(\\.ifh|\\.img|)$',unknown))>0) else
    return(FALSE)
}


print.R4dfp <- function(x,...)
{
  if (!inherits(x,"R4dfp"))
    stop("not a 4dfp image object")

  print(list(
    file=x$file,
    dims=x$dims,
    scale=x$scale,
    mmppix=x$mmppix,
    center=x$center))

  if (!is.null(x$direct.read))
    print(list(direct.read=x$direct.read))

  if (!is.null(x$direct.write))
    print(list(direct.write=x$direct.write))
}


close.R4dfp <- function(con,...)
{
  if (!inherits(con,"R4dfp"))
    stop("not a 4dfp image object")

  if (is.null(list(...)$save))
    save <- FALSE
  else
    save <- list(...)$save

  .Call("close_4dfp",con,as.logical(save),PACKAGE="R4dfp")
}


voxel.to.coord.4dfp <- function(object,voxel)
{
  if (!inherits(object,"R4dfp"))
    stop("not a 4dfp image object")

  if (is.null(ncol(voxel)))
    voxel <- floor(as.matrix(t(voxel[1:3]))) else
    voxel <- floor(as.matrix(voxel[,1:3]))

  expand <- as.matrix(array(1,c(nrow(voxel),1)))

  center <- expand%*%t(object$center)
  scale  <- expand%*%t(object$scale[1:3])
  center[,3] <- center[,3]+scale[3]*object$dims[3]
  return((center-(voxel-1)*expand%*%c(1,-1,1)*scale)*expand%*%c(1,-1,-1)-scale/2)
}


coord.to.voxel.4dfp <- function(object,coord)
{
  if (!inherits(object,"R4dfp"))
    stop("not a 4dfp image object")

  if (is.null(ncol(coord)))
    coord <- as.matrix(t(coord[1:3])) else
    coord <- as.matrix(coord[,1:3])

  expand <- as.matrix(array(1,c(nrow(coord),1)))

  center <- expand%*%as.matrix(t(object$center))
  scale  <- expand%*%as.matrix(t(object$scale[1:3]))
  center[,3] <- center[,3]+scale[3]*object$dims[3]
  return(round((center-(coord+scale/2)*expand%*%c(1,-1,-1))*expand%*%c(1,-1,1)/scale)+1)
}
