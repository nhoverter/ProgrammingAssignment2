## makeCacheMatrix and cacheSolve calculate the inverse of a matrix and cache 
## the result in memory in order to avoid having to repeatedly calculate
## the inverse of the same matrix

## makeCacheMatrix creates an R object that stores a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
      x <<- y
      i <<- NULL
      }
      get <- function() x
      setinv <- function(inv) i <<- inv
      getinv <- function() i
      list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## cacheSolve populates and/or retrieves the inverse from makeCacheMatrix

cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
          message("getting cached data")
          return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}
