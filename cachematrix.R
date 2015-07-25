## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the value of matrix inverse to NULL                   
    m <- NULL
    ## Declare another function where value will be cached in
    set <- function(y){                     
        x <<- y
        ## To change the value of inverse of matrix if matrix had changed
        m <- NULL
    }
    ## Get the value of inverse
    get <- function() x
    ## calculates teh inverse of matrix
    setinverse <- function(solve) m <<- solve
    ## gets the inverse
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   
}


## Cachesolve computes the inverse. If inverse already has been calculated and
## matrix has not changed then cachesolve should retrieve the inverse from each
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    ## If inverse exist then get data from Cache matrix
    if(!is.null(m)){
        message("Getting Cached Data")
        return(m)
    }
    ## If it does not exist, it calcultes and then retreived
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setinverse(m)
    m
}
