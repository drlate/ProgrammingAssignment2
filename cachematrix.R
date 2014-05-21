## makeCacheMatrix() will create a special matrix object from an existing
## matrix object to enable caching of its own inverse.  A matrix object
## that is valid using R's is.matrix() functtion must be passed to it.
##
## usage: specialMatrix <- makeCacheMatrix(matrix)
##        where matrix is an inversible matrix object
##        ## use set() to input a new matrix
##        ## use get() to obtain the existing matrix
##        ## use setInverse() to manually cache a value
##        ## use getInverse() to return the current cached value

makeCacheMatrix <- function(x = matrix()) {
    # initialize an empty cache upon creation of special matrix  
    myInverse <- NULL
    
    set <- function(y){
        # exit if argument is not a matrix
        if(!is.matrix(y)){
            return(message("...not a valid matrix, try again..."))}
        
        # overwrite existing matrix with new matrix
        x <<- y
        
        # set cache to NULL if a new matrix is created using set()
        myInverse <<- NULL
    }
    
    # retrieve existing matrix x 
    get <- function() x
    
    # insert inverse into cache
    setInverse <- function(inverse) myInverse <<- inverse
    
    # retrieve existing cache
    getInverse <- function() myInverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve() will return the inverse of a special matrix created
## using makeCacheMatrix().  It will return the value from cache if
## the inverse has already been calculated.  Otherwise, it will
## calculate the inverse and store the value in cache.
##
## usage: cacheSolve(specialMatrix)
##        where specialMatrix is a matrix created by makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ## retrieve the value of the cache in x and return that value
    ## if it exists (not Null)
    i <- x$getInverse()
    if(!is.null(i)){
        message("...retrieving from cache...")
        return(i)
    }
    
    ## if cached value does not exist (is null), then get the value
    ## of the matrix from specialMatrix and use the solve function
    ## to return the inverse i and store it in cache
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}