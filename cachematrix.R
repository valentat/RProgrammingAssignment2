## This file contains functions implementing objects that hold
## a matrix and its inverse in cache so it is needed to be
## calculated only once a then retrieved from the cache.


## Wrap a plain matrix with inverse caching mechanism.

makeCacheMatrix <- function(x = matrix()) {
    ## x is a square and invertible matrix.
    ##
    ## Return matrix inverse caching object.

    # Cached matrix inverse or NULL if not calculated yet.
    inv <- NULL

    # Set new original matrix and clear previous cached inverse.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # Get the original matrix.
    get <- function() x

    # Store the inverse matrix to the cache.
    setinverse <- function(inverse) inv <<- inverse

    # Return cached matrix inverse or NULL if not calculated yet.
    getinverse <- function() inv

    # Return the caching object with appropriate methods.
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## Calculate a matrix inverse of the object returned by
## makeCacheMatrix() or get it from cache if already calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'.
    ## Additional arguments are passed to solve() during the
    ## first call of the function.

    inv <- x$getinverse()
    if(!is.null(inv)) {
        # Cached inverse available, return it.
        message("Getting cached matrix inverse")
        return(inv)
    }

    # No cached inverse, we must calculate it first.
    data <- x$get()
    message("Calculating matrix inverse")
    inv <- solve(data, ...)

    # And store to the cache.
    x$setinverse(inv)
    inv
}
