## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. This pair of functions cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    cached_matrix <- NULL
    # cached_matrix will be our 'cached matrix' and it's reset to NULL every
    # time makeCacheMatrix is called
    
    # note these next three functions are defined but not run when makeCacheMatrix is called.
    # instead, they will be used by cacheSolve() to get values for x or for
    # cached_matrix (solve) and for setting the inverse matrix.
    
    set <- function(y) {
        x <<- y
        cached_matrix <<- NULL
    }
    
    get <- function() x
    # this function returns the value of the original matrix
    
    setinverse <- function(solve) cached_matrix <<- solve
    # this is called by cacheSolve() during the first cacheSolve()
    # access and it will store the value using superassignment
    
    getinverse <- function() cached_matrix
    # this will return the cached value to cacheSolve() on subsequent access
    
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
    # This is a list of
    # the internal functions ('methods') so a calling function
    # knows how to access those methods.

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

    # the input x is an object created by makeCacheMatrix
    inverse_matrix <- x$getinverse()
    # accesses the object 'x' and gets the value of the mean

    if(!is.null(inverse_matrix)) {
        # if inverse was already cached (not NULL) ...
        
        message("getting cached data")  # ... send this message to the console
        return(inverse_matrix)
        # ... and return the inverse ... "return" ends
    }

    data <- x$get()
    # we reach this code only if x$getinverse() returned NULL

    inverse_matrix <- solve(data, ...)
    # if inverse_matrix was NULL then we have to calculate the inverse

    x$setinverse(inverse_matrix)
    # store the calculated inverse value in x

    inverse_matrix
    # return the inverse to the code that called this function

}
