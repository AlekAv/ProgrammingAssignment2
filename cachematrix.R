## This pair of functions is able to calculate and store 
## the inverse of a matrix for faster access.
## The matrix is assumed to be invertible.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse 
        
        ## Saves the objects into the list.
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(z, ...) {

        inverse <- z$getinverse()
        
        ## Checks if cached value exists and returns it.
        if(!is.null(inverse)) {
                message("Getting cached solution")
                return(inverse)
        }
        
        ## Solves, stores and returns the inverse value otherwise.
        inverse <- solve(z$get(), ...)
        z$setinverse(inverse)
        inverse
}
