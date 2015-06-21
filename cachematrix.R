## This set of functions cache the inverse of a matrix to avoid performing
## time-consuming computations on data that hasn't changed. using a cache
## it's not necessary to compute repeteadly the inverse of a Matrix if it
## has been computed before.

## It was tested on a 2000 x 2000 matrix with 4 million random normal 
## variables, the cache performed much faster than computing the inverse
## of the matrix again.


## The 'makeCacheMatrix' function creates a special 'matrix' in the form of 
## a list containing the four functions needed for setting and getting the
## matrix and it's cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## This object holds the computed inverse matrix
    
    ## This function sets the value of the matrix, resetting the computed
    ## inverse when a new matrix is stored.
    set <- function(y) {
        x <<- y      ## Original data matrix.
        m <<- NULL   ## The inverse is cleared.
    }
    
    ## This function returns the data matrix.
    get <- function() x
    
    ## This function sets the value of the computed inverse matrix in the 
    ## cache 'm'.
    setInverse <- function(inverse) m <<- inverse
    
    ## This functions returns the data of the inverse matrix.
    getInverse <- function() m
    
    ## The special 'matrix' object is created as a list of functions.
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The 'cacheSolve' function computes the inverse of the matrix returned by
## the 'makeCacheMatrix' function. If the inverse has already been calculated
## it retrieves it from the cache, otherwise the inverse is computed.

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## Check if the inverse has been computed (!NULL) and prints a message
    ## to inform that the inverse it's been retrieved from the cache and 
    ## returns the inverse matrix 'm'.
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    
    ## Otherwise the inverse of the matrix is computed.
    data <- x$get()         ## The matrix is retrieved from the special 'matrix' object.
    m <- solve(data, ...)   ## The inverse is computed.
    x$setInverse(m)         ## The inverse is stored in the special 'matrix'
    m                       ## The inverse is returned.
}
