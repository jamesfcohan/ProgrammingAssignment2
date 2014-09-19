## Put comments here that give an overall description of what your
## functions do

## this function creates a special matrix that is a list containing
## functions to set/get the matrix and set/get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
   
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    
    list(set = set, get = get, setInverse=setInverse, getInverse = getInverse)
}


## computes inverse of special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}
