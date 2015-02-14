#############################################################################
# cachematrix
#
# Implements a pair of functions to create a matrix wrapped as an object
# with the inverse cached on first computation.
#
# How to Use:
#   b <- matrix(c(1,1,1,0), nrow=2, ncol=2)   # create sample matrix
#   c <- makeCacheMatrix(b)                   # wrap it into a CacheMatrix
#   cacheSolve(c)                             # first time calculates + caches
#   cacheSolve(c)                             # subsequent are from cache
#############################################################################


# makeCacheMatrix
#   implements a matrix wrapper that caches the matrix and one computed
#   attribute (the inverse in this case).
makeCacheMatrix <- function(x = matrix()) {
	cache <- NULL

	# store the source matrix
	set <- function(y) { x <<- y; cache <<- NULL }

	# retrieve the source matrix
	get <- function() { x }

	# store the inverted matrix in the cache
	setInverse <- function(inverse) { cache <<- inverse}

	# retrieve the inverted matrix from the cache
	getInverse <- function() { cache }

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	}


# cacheSolve
#   retrieve the inverse of a CacheMatrix from cache if available and
#   compute, cache, and retrieve otherwise
cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	if ( is.null(inverse) ) {
		data <- x$get()
		inverse <- solve(data, ...)
		x$setInverse(inverse)
		message("computing, caching, and returning inverse")
	}
	else {
		# returning from cache
		message("returning cached inverse")
	}

	# return the cached or computed+cached inverse
	return (inverse)
}
