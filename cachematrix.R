##
## Two functions are included in this cachematrix file. 
##
## The first function - makeCacheMatrix - returns an in-memory list comprising functions
## to save and return a matrix and the inverse of that matrix.
##
## The second function - cacheSolve - will return the inverse of a matrix, using the cache
## if it has been populated, or by recalculating the inverse if the cache has not been set.

## Prepare and return a list of functions for setting and retrieving moriginal and inverse matrix.

makeCacheMatrix <- function(sourcematrix = matrix()) {
    i <- NULL
    set <- function(y = matrix()) {
        sourcematrix <<- y
        i <<- NULL
    }
    get <- function() sourcematrix
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return the inverse of a matrix from the cache if present, otherwise recalculate the inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached inverse")
        return(i)
    }
    rawmatrix <- x$get()
    i <- solve(rawmatrix, ...)
    x$setinverse(i)
    i
}




