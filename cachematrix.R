## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## function that returns a list with functions bind to
## set(), get(), setinverse(), getinverse()
makeCacheMatrix <- function(x = matrix()) {
	## initialize the matrix to NULL
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	## get() returns the matrix
	get <- function() x

	## setinverse() sets the inverse of the matrix to be the supplied value
	setinverse <- function(inv) i <<- inv

	## getinverse() returns the inverse
	getinverse <- function() i

	## returns a list containing these 4 functions bind
	## to set(), get(), setinverse(), getinverse()
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	## see if the inverse of this matrix has been cached by
	## calling getinverse(), returns true if cached
	i <- x$getinverse()

	## if cached, returns it right away without calling solve()
	if (!is.null(i)) {
		return(i)
	}

	## gets the matrix
	data <- x$get()

	## compute the inverse of the matrix and store the inverse
	## by calling setinverse()
	i <- solve(data, ...)
	x$setinverse(i)

	## return the inverse
	i
}
