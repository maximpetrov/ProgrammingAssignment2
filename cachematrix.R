## These functions implements invering (solving) matrix with caching

## This function take some matrix as an argument and return
## special "matrix" which can store cached inverse

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() {
		x
	}
	setSolve <- function(solve) {
		s <<- solve
	}
	getSolve <- function() {
		s
	}
	list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## This function calculates inverse of matrix with caching result
## First argument is special "matrix" with internal cache
## which can be created by makeCacheMatrix function
## Other arguments are the same as for solve function

cacheSolve <- function(x, ...) {
        s <- x$getSolve()
	if (!is.null(s)) {
		message("getting cache data")
		return(s)
	}
	message("solving matrix")
	data <- x$get()
	s <- solve(data, ...)
	x$setSolve(s)
	s
}
