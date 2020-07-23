## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix that can cache its inverse

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

## This function computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
         if (!is.null(i)) {
                  message("getting cached data")
                  return(i)
         }
         data <- x$get()
         i <- solve(data, ...)
         x$setinverse(i)
         i
}
