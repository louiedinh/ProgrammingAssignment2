## A pair of functions for wrapping matrices to make repeated computation of inverses more efficient.

## makeCacheMatrix :: matrix => CacheMatrix
## cacheSolve :: matrix => matrix

## makeCacheMatrix(x)
# Params:
#   x - A matrix
# Returns:
#   A CacheMatrix that can store it's own inverse to save computations on 
#   future solves.
#
# API of CacheMatrix:
#   Constructor:
#     m <- makeCacheMatrix(x = matrix(....))
#   Methods:
#     m$get #=> Returns the matrix x
#     m$setinverse #=> Caches the inverse of m
#     m$getinverse #=> Returns the saved inverse, could be NULL

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) m <<- mean
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve(x, ...) => Returns the inverse of x.
# Params: 
#   x - An invertible matrix
#   ... - Arguments to the solve() function
# Returns:
#   The inverse of x. If this is the first time cacheSolve is called on x,
#   the inverse will be computed and cached. Subsequent calls
#   will retrieve the cached value.


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        ## Return a matrix that is the inverse of 'x'
        x$setinverse(i)
        i
}
