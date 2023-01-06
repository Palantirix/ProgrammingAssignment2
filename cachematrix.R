## functions designed to create matrix object and cache it's inverse.
## second function checks if inverse is available and retrieves from cache, or computes

#function creates a special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setmatrix <- function(solve) m <<- solve
     getmatrix <- function () m
     list(set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)
}

#function computes the inverse of the special 'matrix' returned by makeCacheMatrix
#if the inverse has already been calculated then retrieve from cache
cacheSolve <- function(x, ...) {
     m <- x$getmatrix()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setmatrix(m)
     m
}
