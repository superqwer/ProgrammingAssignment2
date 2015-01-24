## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # makes object containing matrix, cache for its inverse
    # and corresponding methods 
    r <- NULL
    set <- function(y){
        x <<- y
        r <<- NULL
    }
    get <- function() x
    setReversed <- function(reversed) r <<- reversed
    getReversed <- function() r
    list(
        set=set, 
        get=get, 
        setReversed=setReversed, 
        getReversed=getReversed
    )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # solving a %*% x == b, where b is identity matrix  
    # gives us reversed 'a' matrix
    r <- x$getReversed()
    if(!is.null(r)){
        message("getting cached data")
        return(r)
    }
    data <- x$get()
    r <- solve(data)
    x$setReversed(r)
    r
}
