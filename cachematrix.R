## The below R functions cache the computation of Matrix Inversion  


## makeCacheMatrix function creates a matrix object caching its inverse


makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinv <- function(solve) invm <<- solve
    getinv <- function() invm
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve function computes the inverse of the matrix object returned by the makeCacheMatrix function
## However, if the matrix inversion is already calculated this function retrieves it from the cache instead of re-computing it.

cacheSolve <- function(x, ...) {
    invm <- x$getinv()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinv(invm)
    invm
}