## These two functions (makeCacheMatrix and cacheSolve) are used to create
## a special 'matrix' object that will cache its inverse to avoid recomputing
## it each time it is needed.
##
## Note: it is assumed that the matrix is invertible.

## 'makeCacheMatrix' creates a special 'matrix' object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(imat) inv <<- imat
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## 'cacheSolve' will compute the inverse matrix of the special 'matrix' object
## the first time (or if the matrix has changed), it returned the cached
## inverse matrix otherwise.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("--- retrieve cached inverse ---")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}
