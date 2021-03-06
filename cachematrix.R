## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix() creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## cached inverse initially set to NULL
  I <- NULL
  
  ## set original matrix, clear cached inverse
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  
  ## return original matrix
  get <- function() x
  
  ## set cached inverse value
  setInv <- function(sol) I <<- sol
  
  ## return cached value
  getInv <- function() I
  
  ## return list of functions
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve() computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## access the cached value
  I <- x$getInv()
  
  ## check for existing cached inverse
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  
  ## no cached inverse, so solve for original and store inverse
  I <- solve(x$get(), ...)
  x$setInv(I)
  
  #return inverse
  I
}
