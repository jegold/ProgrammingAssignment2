## function makeCacheMatrix creates a list of functions (see decriptions below) 
## that may be used at the console or by the function cacheSolve() or other 
#  functions in the global environment.  Through the function, setinverse, it 
## caches the inverse of a matrix through the variable, m.  m is a cache by
## virtue of the fact that is it accessible from the parent level of its calling
## function.  Note that the function set allows access of the matrix x in the 
## parent environment.  This function also clears the cache, m.
##
## The function cacheSolve() returns the inverse of a matrix x (see below for
## further information).
## 
## makeCacheMatrix creates a list of functions to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. calculate the inverse of the matrix
##  4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
        list (set = set, get = get, 
              setinverse = setinverse,
              getinverse = getinverse)
}
## cacheSolve returns a matrix that is the inverse of function argument x.  First,
## it checks in the cache to see if the inverse has already been calculated.  
## If not, the matrix is retrieved and the inverse is calculated and printed out.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
