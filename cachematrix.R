# cachematrix.R implements a special type of data structure (implemented as a list)
# that caches the results of the solve() R function.  Implemented as two
# functions, makeCacheMatrix() acts as a constructor for this data structure
# and cacheSolve() checks the data structure returning the cached
# solution if available and solving and caching if not.

# makeCacheMatrix() constructs a caching data structure for a matrix.  Intended
# to be used with the cacheSolve() function defined below.
#
# Arguments:
#            x -- a matrix that is assumed to be invertible
#
# Returns:
#            A list that contains a placeholder for the cached value (initialized
#            to NULL) and functions that can set the cached value and get the
#            cached value.
makeCacheMatrix <- function(x = matrix()) {

    # Intialize the cached value
    m <- NULL

    # A function to get the given matrix
    get <- function() x

    # A function that caches the inverse matrix (result from solve())
    setinverse <- function(inverse) m <<- inverse

    # A function to return the cached value
    getinverse <- function() m

    # Return our data structure as a list, binding the
    # above functions to it
    list(get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# cacheSolve() computes and caches the inverse of the given
# matrix 'x'.  If the cached value is available, that is 
# returned otherwise solve() is called on the matrix, the
# return from resolve() cached and then returned.
#
# Arguments:
#            x -- a data structure returned from makeCacheMatrix()
#
# Returns:
#            The inversion of the matrix contained in 'x'.  The
#            matrix contained in 'x' is assumed to be invertible.
cacheSolve <- function(x, ...) {

    # Get the cached value and return it
    # if it's been set (not NULL)
    m <- x$getinverse()
    if (!is.null(m)) {
      message("Getting cached data")
      return(m)
    }

    # The result has not been cached, get
    # the original matrix, call solve() on it
    # cache the result and return the result
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
