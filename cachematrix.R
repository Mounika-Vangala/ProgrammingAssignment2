## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix named 'makeCacheMatrix' that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invrs <<- inverse
    getInverse <- function() invrs
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## This function computes the inverse of the above created 'makeCacheMatrix' matrix. If the inverse is already calculated then it gives the resultant inverse matrix from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invrs <- x$getInverse()
    if(!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    data <- x$get()
    invrs <- solve(data, ...)
    x$setInverse(invrs)
    invrs
}
