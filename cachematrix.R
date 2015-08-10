## This pair of functions is able to calculate and store 
## the inverse of a matrix for faster access.

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
        
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

## The matrix is assumed to be invertible.

cacheSolve <- function(z, ...) {

        inverse <- z$getinverse()
        if(!is.null(inverse)) {
                message("Getting cached solution")
                return(inverse)
        }
        inverse <- solve(z$get(), ...)
        z$setinverse(inverse)
        inverse
}
