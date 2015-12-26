## This pare of functions are for the creation of a special "matrix" object 
## and caching the inverse of the matrix. 

## This function creates a special "matrix" object x that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    # x is the matrix forwarded as a parameter of the function
    
    # ix is the inverse matrix
    ix <- NULL
    
    # function to set the matrix x in a different environment
    set <- function(y) {
        x <<- y
        ix <<- NULL
    }
    # function to get the matrix x
    get <- function() x
    
    # function to set the inverse matrix ix in a different environment
    setinv <- function(inv) ix <<- inv
    
    # function to get the inverse matrix ix
    getinv <- function() ix
    
    # list all available functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    # get the inverse matrix ix
    ix <- x$getinv()
    
    # if inverse matrix ix is return ix
    if(!is.null(ix)) {
        message("getting cached data")
        return(ix)
    }
    
    # get matrix x
    data <- x$get()
    
    # calculate inverse matrix ix
    ix <- solve(data, ...)
    
    # set inverse matrix ix
    x$setinv(ix)
    ix
}
