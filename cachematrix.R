


## Caching the Inverse of a Matrix:
## This function creates a special "matrix" object that can cache its inverse.
## The benefit of caching the inverse of a matrix is to save the computational 
##Power. 


makeCacheMatrix <- function(x = matrix()) {

        Inverse <- NULL
        set <- function(y) {
                x <<- y
                Inverse <<- NULL
        }
       
get <- function() x

        setInverse <- function(inverse) Inverse <<- inverse
    
# The get() function return the Matrix once initilised else return NULL.
        getInverse <- function() Inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

        Inverse <- x$getInverse()
        if (!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        mat <- x$get()

# Solve Function return the inverse of Matrix and load in the Inverse 
        Inverse <- solve(mat, ...)
# setInverse Function set the Matrix Inverse
        x$setInverse(Inverse)
        Inverse
}
