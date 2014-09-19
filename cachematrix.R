## The functions provide a way to cache and reuse matrix inversion result

## The function creates a special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inverseValue) inverse <<- inverseValue
	getInverse <- function() inverse
	list(set =set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The function returns the cache of the special matrix object created by makeCacheMatrix function
## If the inverse has already been calculated, then the cached value is returned.
## Otherwise the inverse is calculated, put into the cache and returned

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message("getting the cached inverse matrix")
		return (inverse)
	}
	matrixData <- x$get()
	inverse <- solve(matrixData)
	x$setInverse(inverse)
	inverse
}
