## Functions give an ability to cache inverse of a matrix
## (as to compute inverse can be quite time - consuming)
## 

## makeCacheMatrix creates a special 'matrix' type
## matrix can be stored (get()), updated (set()) as long as with its inverse

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


## cacheSolve checks if inverse was already computed
## if this is the case the inverse from cache is return
## otherwise inverse is computed

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

