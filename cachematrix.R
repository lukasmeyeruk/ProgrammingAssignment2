## The function will compute the inverse of a matrix and will the store it in the cache.
## If the inverse of that matrix has already been computed the function will retrieve and return the matrix from the cache.

## This function creates a special matrix with additional functions that allow storing or retrieving a matrix from cache

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinversed = getinverse)
}


## This function computes the inverse of a matrix and then stores the result in the cache.
## If the result already exists in the cache it will retrieve and return the inverse from the cache (without computing it again)


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
