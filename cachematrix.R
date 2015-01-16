## makeCacheMatrix creates a special "matrix", which is  a list containing
## the following functions:

## set: receives  a matrix 'y' as input and sets the local atribute 'x' to it.
## get: returns the value of the matrix 'x'
## setinv: sets the value of the inverse
## getinv: returns the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y     # x is in the parent environment of 'set', i.e. 'makevector'
        m <<- NULL  # same case for m
    }
    get <- function() x
    setinv <- function(inv) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix" 
## created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
