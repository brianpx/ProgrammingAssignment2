## the function makeCacheMatrix builds a matrix object then cacheSolve calculates 
## the inverse of the matrix.
## If the matrix inverse has already been calculated, it will find 
## ithe cache and return it and not recalculate it.

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  get <- function() x
  setsoln <- function(soln) ix <<- soln
  getsoln <- function() ix
  list(set = set, get = get,
       setsoln = setsoln,
       getsoln = getsoln)
}

## Return a matrix that is the inverse of 'x'
## If the matrix inverse has already been calculated, it will find 
## ithe cache and return it and not recalculate it.  ix is inverse of x

cacheSolve <- function(x, ...) {

  ix <- x$getsoln()
  if(!is.null(ix)) {
    message("getting cached data")
    return(ix)
  }
  data <- x$get()
  ix <- solve(data, ...)
  x$setsoln(ix)
  return(ix)
}
