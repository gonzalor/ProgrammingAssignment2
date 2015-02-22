## Program Assignment 2 - Programming R
## Student: Roberto Gonzalo Rodriguez

## Creates a Function that wraps a matrix whose inverse will be cached
## Encapsulates the matrix and the inverse matrix and provides access
## functions to get and set these values

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## Set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Get the value of the matrix
    get <- function() {
        x
    }
    
    ## Set the value of the inverse matrix
    setsolve <- function(solve) {
        m <<- solve
    }
    
    ## Get the value of the inverse matrix
    getsolve <- function() {
        m
    }
    
    ## Returns a list containing the 4 functions defines before
    list(
        set = set, 
        get = get, 
        setsolve = setsolve, 
        getsolve = getsolve)
}


## Returs the cached inverse matrix of 'x'
## Calculates the inverse of the matrix only once the first time it is required
## NOTE: The matrix supplied it is suposed to be Invertible.

cacheSolve <- function(x, ...) {
    ## Tries to get the Inverse Matrix 
    m <- x$getsolve()
    
    ## If it is not NULL means that it was calculated before (cached)
    if (!is.null(m)) {
        message("Getting cached data")
        return (m)
    }
    
    ## If the execution reaches here, it means that it is the first time
    ## the inverse matrix was required
    data <- x$get()
    
    ## Calculates the inverse matrix
    m <- solve(data, ...)
    
    ## Stores the inverse matrix in the cached
    x$setsolve(m)
    
    ## Returns the inverse matrix calculated
    m
}
