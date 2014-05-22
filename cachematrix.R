## These functions cache the inverse of a matrix, to be able to look up the 
## results instead of recalculating it, if it has already been calculated.
## This can help reduce the number of potentially time-consuming calculations
## that need to be done.

## The makeCacheMatrix function can take a matrix, and set it as x, and clears 
## the cache (m). You can run get() to print out the matrix that was stored.
## Setinverse is used in cacheSolve to save the inverse in the global 
## environment. Getinverse is used to pull the saved inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function checks to see if the inverse of the matrix is saved
## in the cache. If it is, it prints out the inverse of the matrix 
## [getinverse()]. If it isn't saved, it calculates the inverse [solve(data,...)]
## and saves the inverse to the cache [setinverse()].

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
