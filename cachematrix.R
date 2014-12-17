## Put comments here that give an overall description of what your
## functions do

## cache Matrix and cache its inverse
makeCacheMatrix <- function(x = matrix()) {

	inverse <- NULL

	## get & set matrix
	get <- function() x
	set <- function(y) x <<- y

	## get & set inverse
	getInverse <- function() inverse
	setInverse <- function(inv) inverse <<- inv

	list( set = set, get = get,
		getInverse = getInverse,
		setInverse = setInverse)
}


## calc the matrix inverse by using cache function above
cacheSolve <- function(x, ...) {

    # if inverse already cached, simple return
	inv <- x$getInverse()
	if( !is.null(inv) ) {
		return(inv)
	}

	# if inverse not cached, calc it and cache it, final return
	data <- x$get()
	tmp_inv <- solve(data)
	x$setInverse(tmp_inv)
	tmp_inv
}
