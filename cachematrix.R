## ----------------------------------------------------------------------------
## Programing Assignment Part 2
##
## The methods bellow can be used to minimize the execution time for code that
## uses the same inverted matrix multiple times.
## Instead of calculating it every time the value can be cached and retrived
## using a system based on scoping rules.
## ----------------------------------------------------------------------------

## This function creates a list of functions that can be used to manage
## the caching of a inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## If we're setting the function list for the first time the inverted
        ## matrix is empty
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL ## If we change the matrix we need to invalidate
                             ## the inverted one we already have
        }
        ## Get the matrix without the function list so we can apply
        ## functions to it.
        get <- function() x
        ## Cache the inverted matrix
        setsolve <- function(inverted) inv <<- inverted
        ## Return the inverted matrix
        getsolve <- function() inv
        ## The list of functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function returns the inverted matrix
## 1) If the inverted matrix is available than it returns the cached one
## 2) If the inverted matrix is not available/has never been calculated then it
## calculates and stores it

cacheSolve <- function(x, ...) {
        ## Before we invert the matrix check if we have one cached.
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("Getting Cached Inverted Matrix")
                return(inv)
        }
        ## We need to create a new inverted matrix and cache it
        data <- x$get()
        inv <- solve(data,...)
        x$setsolve(inv)
        inv
}
