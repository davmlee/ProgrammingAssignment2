# The makeCacheMatrix function creates a special "matrix" object
# that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      invr <- NULL
      set <- function(y) {
              x <<- NULL
              invr <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) invr <<- inverse
      getinverse <- function() invr
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
    
}
# The cacheSolve function computes the inverse of the special
# "matrix returned by the makeCacheMatrix function above. If
# the inverse has already been calculated (and the matrix has
# not changed), then the cacheSolve function will retrieve the
# inverse from the cache (with message that it is doing so).
cacheSolve <- function(x, ...) {
      invr <- x$getinverse()
      if(!is.null(invr)) {
              message("geting cached data!")
              return(invr)
      }
      data <- x$get()
      invr <- solve(data)
      x$setinverse(invr)
      invr
}
