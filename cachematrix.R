## Objective of the program is to compute matrix inverse, store and re-use
## the result from cache without having to compute every time needed.

## The first function, makeCacheMatrix() creates a special "vector", 
## which is really a list containing a function to
## set the value of the matrix , get the value of the matrix
## set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve ## Solve() function used to compute inverse of a matrix
    getsolve <- function() s
    list(set = set, get = get, 
         setsolve = setsolve,
         getsolve = getsolve) 
}


## The following function calculates the inverse of the matrix 
## created with the makeCacheMatrix() function. However, it first checks to see if 
## the inverse has already been computed. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse 
## of the matrix using the solve() function and prints result.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
## "If" construct is used to check if the inversed matrix has been calculated or not.
    if(!is.null(s)) {
        message("getting cached data") ## Prints the message if cache has inversed matrix
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...) ## Solve function used to compute inverse
    x$setsolve(s)
    s ## Returns a matrix that is the inverse of 'x'
}
