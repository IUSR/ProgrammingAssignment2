## A set of functions that aids frequent access of results of matrix inversion operations,
## best fit for matrices with large dimensions.

## Function to create a special matrix whose inversion is cached for subsequent access

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function to retrieve the inversion of any matrix created via makeCacheMatrix if already
## cached, otherwise calculcates the inversion and cache it for the matrix

cacheSolve <- function(x, ...) {
    ## returns NA if x is not properly created via makeCacheMatrix
    if(!"getinv" %in% names(x) | !"setinv" %in% names(x)) {
        warning("input matrix not wrapped by makeCacheMatrix")
        return(NA)
    }
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...) %*% data
    x$setinv(i)
    i
}
