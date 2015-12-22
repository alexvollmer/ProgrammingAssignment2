## Calculating the inverse of a given matrix can get expensive if
## performing the same inversion over and over. The pair of functions
## below provide a means to solve the inversion of a given matrix once
## and cache the result which may improve performance when applied to a large
## number of matrices.

## Wraps a given matrix in a list of functions to capture and return
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Retrieves the cached inversion of a wrapped matrix if available, or
## calculates it upon first request. This function assumes that its sole
## argument is a "wrapped" matrix from a call to makeCacheMatrix()

## Example:
## m <- matrix(runif(9), 3, 3)
## mx <- makeCacheMatrix(m)
## cacheSolve(mx)  ## computes inverse
## cacheSolve(mx)  ## uses cached value

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
