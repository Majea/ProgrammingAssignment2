## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## This pair of functions 'makeCacheMatrix' and 'cacheSolve' cache the inverse of a matrix.
## Here is an example of usage:
## first define some matrix to inverse
## > x <- diag(3)
## > x[1,1] <- 2
## now we create a cache object:
## > y <- makeCacheMatrix(x)
## To get the inverse matrix, we can finally use the cacheSolve method on the y object
## > cacheSolve(y)
## [,1] [,2] [,3]
## [1,]  0.5    0    0
## [2,]  0.0    1    0
## [3,]  0.0    0    1
## Each we'll call the cacheSolve method again, the cache will be returned until we change the matrix 
## we work on by calling:
## > y$set(z)
## z being any other matrix we want to inverse.
## Please notice that a message is displayed each time a cached value is returned.


## The returned object holds the matrix to inverse and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, 
         setsolve = setsolve,
         getsolve = getsolve)
}


## This method returns the inverse of the matrix in the x object.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
