## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## This function creates a special "matrix" object that can cache its inverse.
## Below are two functions that are used to create a special object that stores
## a matrix and caches its inverse.

makeCacheMatrix <-function (x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invs <<-inverse
        getinverse <- function() invs
        list(set=set, 
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" created by 
## function makeCacheMatrix above. If the inverse has already been calculated 
## and the matrix has not changed, then it should retrieve the inverse from the 
## cache.

cacheSolve <- function (x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getinverse()
        if (!is.null(invs)) {
                message("getting cache data")
                return(invs)
        }
        data <- x$get()
        invs <- solve(data,...)
        x$setinverse(invs)
        invs
}

