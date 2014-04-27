## This file contains two functions that are used to create a special object that stores a matrix and caches its inverse.

## This function creates a special object that allows caching the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function calculates the inverse of a matrix. If the inverse has been already calculated and cached it does not
## recalculate it, but instead it returns the cached value.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) { # if there is cached inverse, return it
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...) # calculate the inverse if it was not cached already...
    x$setsolve(s)         # and cache it for using in the future
    s
}
