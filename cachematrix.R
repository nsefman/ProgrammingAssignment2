# To avoid recalculating matrix inverses, the functions bellow allow caching inverses when they are calculated for the first time, so they can be retrieved later without recalculating


# function to create vector of functions (individualy commented bellow)
makeCacheMatrix <- function(x = matrix()) {
# first, let's clear cache
        m <- NULL
# set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
# get the matrix
        get <- function() x
# set the inverse of the matrix
        setsolve <- function(solve) m <<- solve
# get the inverse of the matrix
        getsolve <- function() m
# return list of functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# function to return the inverse of the matrix (from cache if possible, otherwise calculated) 
cacheSolve <- function(x, ...) {
# get inverse from cache
        m <- x$getsolve()
# check the result - if it is not null, return it and finish
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
# if the result was null, get the matrix...
        data <- x$get()
# ...calculate the inverse...
        m <- solve(data, ...)
# ...and store the calculated inverse
        x$setsolve(m)
# return the result
        m
}
