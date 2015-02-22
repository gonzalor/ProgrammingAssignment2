## Program Assignment 2 - Programming R
## Student: Roberto Gonzalo Rodriguez

## Creates a Function to store a matrix whose inverse will be cached

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setsolve <- function(solve) {
        m <<- solve
    }
    
    getsolve <- function() {
        m
    }
    
    list(
        set = set, 
        get = get, 
        setsolve = setsolve, 
        getsolve = getsolve)
}


## Returs the cached inverse matrix of 'x'

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if (!is.null(m)) {
        message("Getting cached data")
        return (m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
