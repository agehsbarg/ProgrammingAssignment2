## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(M = matrix()) {

        Minv <- NULL
        set <- function(A) {
                M <<- A
                Minv <<- NULL
        }
        get <- function() M
        setinverse <- function(inv) Minv <<- inv
        getinverse <- function() Minv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(M, ...) {
        ## Return a matrix that is the inverse of 'M'

        Minv <- M$getinverse()
        if(!is.null(Minv)) {
                message("getting cached data")
                return(Minv)
        }
        data <- M$get()
        Minv <- solve(data, ...)
        M$setinverse(Minv)
        Minv
}

