## makeCacheMatrix stores a matrix and caches its inverse.
## cacheSolve returns the inverse of a matrix created with makeCacheMatrix.


# makeCacheMatrix: Stores a matrix and caches its inverse.
#
# Returns a list of functions:
#     set() - set the value of a matrix and set its inverse to NULL
#     get() - get the matrix
#     setinv() - set (cache) the inverse of the matrix (not calculated!)
#     getinv() - get the inverse of the matrix
#
# Warning: Does not check whether the matrix is square or singular.
#          setinv() should only be called from cacheSolve(), as error checking
#          takes place there.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
  
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inverse) inv <<- inverse
        
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Returns the inverse of matrix 'x' created with makeCacheMatrix.
#
# First checks whether the matrix inverse of x has been cached.
# If the inverse has been cached, the cached value is returned. Otherwise
# the inverse is caclculated and cached.
#
# Errors: The validity of the matrix is not checked, but if it isn't
#         square or singular, solve() will throw an error.
cacheSolve <- function(x, ...) {

        # If the matrix inverse of 'x' is cached, return cached value
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Calculate the inverse of 'x', cache and return it.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
