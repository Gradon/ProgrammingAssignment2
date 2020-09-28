## creates matrix functions that can cache a matrix and its inverse to
## reduce the number of times a frequently-performed operation needs to be
## calculated.

## setmatrix - stores the matrix provided in argument x
## getmatrix - return the matrix stored
## setinverse - calculate and store the inverse of the matrix from argument x
## getinverse - return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  ## function sets the object matrix equal to the provided matrix x
  setmatrix <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  ## function returns the matrix stored in the object
  getmatrix <- function() x
  ## function stores the inverse of the provided matrix 
  setinverse <- function(solve) m <<- solve(x)
  ## function returns the inverse matrix stored in the object
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse,
       getinverse = getinverse)
}


## returns the inverse of the matrix x
## if the inverse of the matrix x has already been computed, retrieve the
## solution using get

cacheSolve <- function(x, ...) 
{
  ## try to get the inverse of the matrix stored in this object
  ## if not null, then the inverse of the matrix has been computed, return it
  m <- x$getinverse()
  if(!is.null(m)) {
    message("retrieving stored inverse")
    return(m)
  }
  ## if you get to this point the inverse matrix is not yet computed
  ## get the original matrix and compute its inverse and store it
  working <- x$getmatrix()
  m <- solve(working, ...)
  x$setinverse(m)
  ## return the inverse matrix
  m
}
