###############################################################################
##
##      The two functions below are to calculate and return
##      the inverse of a given matrix.
##
##      makeCacheMatrix(): creates the object containing the
##      functions to manipulate the matrixes.
##
##      cacheSolve(): returns the inverse of the matrix
##      that is mantained by the object.
##
###############################################################################


## This function creates an object that is a list of 4 functions:
##      set() that caches the original matrix
##      get() that returns the current original matrix
##      setinv() that caches the inverse matrix
##      getinv() that returns the current inverse matrix
makeCacheMatrix <- function(mt = matrix()) {
        inv <- NULL
        set <- function(new) {
                mt <<- new
                inv <<- NULL
        }
        get <- function() mt
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes an object of the type created by makeCacheMatrix()
## and returns the inverse of the original matrix mantained by the object.
##      If an inverse matrix is already cached: it returns it.
##      If no inverse matrix is cached: it calculates the inverse, caches it and returns it.
cacheSolve <- function(x, ...) {
        
        # if the inverse exists
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # if the inverse doesn't exist
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i

}


