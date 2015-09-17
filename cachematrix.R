## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. This is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL                               ##sets the value of the inversed matrix to NULL
        set <- function(y) { 
                x <<- y                         ##sets the value of the matrix
                s <<- NULL                      ##sets the value of the inversed matrix to NULL
        }
        get <- function() x                     ##get the value of the matrix
        setsolve <- function(solve) s <<- solve ##sets the value of the inversed matrix
        getsolve <- function() s                ##gets the value of the inversed matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve will retrieve the 
## inverse from the cache.

cacheSolve <- function(x) {
        s <- x$getsolve()                       ##getting cached matrix
        if(!is.null(s)) {                       ##checking cached matrix
                message("getting cached data") 
                return(s)                       ##returning cached matrix
        }
        data <- x$get()                         ##getting data from "matrix"
        s <- solve(data)                        ##inversing the matrix
        x$setsolve(s)                           ##caching the result
        s                                       ##returning the result
}