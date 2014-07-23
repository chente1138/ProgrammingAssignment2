## This script contains two functions that allow you to cache the inverse of a
## matrix in memory, to a given matrix. So if it has been done previosly, and 
## the matrix is the same matrix, it just gives you the stored inverse and it 
## does not calculate it again.

## makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
           
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     } ## if m has been calculated before, it just gets the calculated inverse
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
          ## These 3 lines calculates and cache the inverse 
     m
}
