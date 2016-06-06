## Put comments here that give an overall description of what your
## functions do

#
# Creates a special matrix object that can cache its inverse.
#
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                mat <<- y
                inv <<- NULL
        }
        get <- function() mat
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


#
# Computes the inverse of the special matrix object returned by
# the makeCacheMatrix function. Returns the cached matrix inverse if 
# the inverse has already been computed.
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
