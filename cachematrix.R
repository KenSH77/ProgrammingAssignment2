## Put comments here that give an overall description of what your
## functions do
## Created by Ken, on Feb 7th, 2017

## Write a short comment describing this function
## Creates a matrix object that has attributes:
##
## set
##     Set value of the matrix
## get
##     Get value of the matrix
## getInv
##     Get inverse of the matrix, returns a cached result if it has
##     already been evaluated
## setInv
##     Set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) m <<- inverse
	getInv <- function() m
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

## Expects `x` to be a `cacheMatrix` object.
## Computes the inverse of `x` if the result is not cached; otherwise it returns a cached result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInv()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setInv(m)
	m
}
