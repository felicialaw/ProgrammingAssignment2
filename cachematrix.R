## Programming Assignment 2: Caching the Inverse of a Matrix
## Allows the caching of a matrix inverse to avoid recomputation when the
## of the matrix have not changed.


## "makeCacheMatrix" function
## creates a list containing functions to
##	1. set the value of the matrix
##	2. get the value of the matrix
##	3. set the value of the inverse
##	4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	f <<- list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
	f
}


## "cacheSolve" function
## uses makeCacheMatrix to return a matrix that is the inverse of x
## by getting the inverse from the cache if this has already been calculated
## otherwise by calculating the inverse then setting this value in the cache

cacheSolve <- function(x, ...) {
	m <- f$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- f$get()
	m <- solve(data, ...)
	f$setinverse(m)
	m
}
