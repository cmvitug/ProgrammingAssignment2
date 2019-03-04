## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv = NULL
      set = function(y) {
            x <<- y
            inv <<- NULL
      }
      get = function() x
      setinv = function(inverse) inv <<- inverse
      getinv = function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cache solve retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cashed data")
            return(inv)
      }
      mat.data <- x$get()
      inv = solve(mat.data, ...)
      x$setinv(inv)
      return(inv)
}
