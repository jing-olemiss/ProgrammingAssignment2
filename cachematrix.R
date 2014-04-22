## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' or "No inverse matrix" if the determinant is zero.
        s <- x$getsolve()
	    if(!is.null(s)) {
	        message("getting cached data")
	        return(s)
	    }
	    data <- x$get()
	    d <- det(data, ...)
	    if (d == 0){
	    	message("The determinant is zero. No inverse matrix.")
	    }else{
	        s <- solve(data, ...)
	        x$setsolve(s)
	        s
	    }
}
