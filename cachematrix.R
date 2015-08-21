## The functions below are used to cache
## and return the inverse of a matrix.

## The first function sets and gets the value of a matrix
## and then sets and gets the inverse of said matrix.

makeCacheMatrix <- function(x = matrix()) {
    iM <- NULL
    set <- function(y) {
      x <<- y
      iM <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) iM <- solve
    getmatrix <- function() iM
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
  }

## This function retrieves the cached inverse matrix 
## if it has previously been computed
## or calculates the inverse if it has not been.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    iM <- x$getmatrix()
    if(!is.null(iM)) {
      message("getting cached data")
      return(iM)
    }
    matrix <- x$get()
    iM <- solve(matrix, ...)
    x$setmatrix(iM)
    iM
  }

