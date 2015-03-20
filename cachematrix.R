## This file defines the framework for cached inverse matrix computation.
## First we need to prepare the cache structure using 'makeCacheMatrix',
## after this, 'cacheSolve' can be used in order to compute inverse of
## the original matrix. If cache is not ready, 'cacheSolve' will compute
## the matrix inverse and store it internally, after this, the cached
## value of the inverse matrix will be availabe without computing
## it second time.
## Here is a simple example:
## > # Define the original martix(s)
## > originalMatrix <- matrix(c(1,2,2,1), 2, 2)
## > # Now, prepare the cache structure (matrix)
## > inverseCache <- makeCacheMatrix(originalMatrix)
## > # Now, we are ready to compute the inverse of the original matrix
## > # First, as the inverse matrix is not available, we will compute it
## > # and store it internally in 'inverseCache' structure.
## > finalInverse1 <- cacheSolve(inverseCache)
## > # When we request the inverse the 2nd time, we will get
## > # the cached value, no need to compute it again.
## > finalInverse2 <- cacheSolve(inverseCache)

## This function prepares cache structure that will be used for
## managing cached value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        cv <- NULL
        set <- function(y) {
                x <<- y
                cv <<- NULL
        }
        get <- function() x
        setcache <- function(cachedValue) cv <<- cachedValue
        getcache <- function() cv
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}


## This function can be used for computing inverse of the input matrix 'x'.
## If the inverse has alreay been calculated, the cached value is returned.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        cv <- x$getcache()
        if(!is.null(cv)) {
                message("getting cached data")
                return(cv)
        }
        data <- x$get()
        cv <- solve(data)
        x$setcache(cv)
        cv
}
