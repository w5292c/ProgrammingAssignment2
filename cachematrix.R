## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
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
