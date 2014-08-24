## The first function creates a special "matrix", which is
## really a list containing a function set the value of the matrix,
## get the value of the matrix, set the value of the mean and 
## get the value of the mean

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
    
    
    

## The following function calculates the mean of the special "matrix"
## created with the above function. But, it first checks to see if the
## mean has already been calculated. If so, it `get`s the mean from the
## cache and skips the computation. Otherwise, it calculates the mean of
## the data and sets the value of the mean in the cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
