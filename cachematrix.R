## The funtions below are able to cache potentially 
## time-consuming computations involved in matrix inversion
## by taking advantage of the scoping rules of the R language. 

## -- CREATES A SPECIAL 'MATRIX' --
## The makeCacheMatrix function creates a special 'matrix'
## that is used to set and get the value of the matrix,
## as well as set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## -- CALCULATES THE INVERSE OF THE 'SPECIAL' MATRIX --
## The cacheSolve function first checks to see if the inverse 
## of the matrix has been calculated. If it has it justs retrieves
## it from cached memory. If not, it is calculated (the inverse)
## and then set as the inverse in cached memory.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  dataMatrix <- x$get()
  m <- solve(dataMatrix, ...)
  x$setinv(m)
  m ## The inverse matrix of x
}
