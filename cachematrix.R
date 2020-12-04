## Caching the inverse of a matrix

## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function( m = matrix() ) {
  i <- NULL
  set <- function(oneMatrix) {
    m <<- oneMatrix
    i <<- NULL
  }
  get <- function() m;
  setInverse <- function(oneInverse) i <<- oneInverse
  getInverse <- function() i
  list(set = set, get = get, 
       setInverse = setInverse, getInverse = getInverse )
}

## # cacheSolve: This function computes the 
## inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix 
## has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  m <- x$get()
  inverse <- solve(m, ...)
  x$setInverse(inverse)
  inverse
}