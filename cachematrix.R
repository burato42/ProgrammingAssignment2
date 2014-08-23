## Below are two functions that are used to create a special object 
## that stores a matrix and cache's its inverse matrix.

## The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to
## set the matrix
## get the matrix
## set the inverse matrix
## get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Function cacheSolve calculates the inverse matrix.
## If inverse matrix been calculated for this matrix it returns cache
## Else function calculates inverse matrix and sets inverse matrix in the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    
}