## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    myInverse <- NULL
    
    set <- function(y){
        # exit if argument is not a matrix
        if(!is.matrix(y)){
            return(message("...not a valid matrix, try again..."))}

        x <<- y
        myInverse <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) myInverse <<- inverse
    getInverse <- function() myInverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
        message("...retrieving from cache...")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}