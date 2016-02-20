## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object that can cache its inverse (inv)
## set(): passed in value y set to x (the value of the matrix)
## get(): returns x (the matrix)
## setinverse(): sets the value of the calculated inverse (cached)
## getinverse(): retrieves value of cached inverse


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	get <- function() x

	setinverse <- function(inverse) inv <<- inverse

	getinverse <- function() inv

	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

	
}


## Calculates inverse of matrix if not already cached, otherwise returns cached inverse

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()

	if(!is.null(inv)) {
		message("getting cached inverse")
		return(inv)
	}

	matrixdata <- x$get()

	inv <- solve(matrixdata) 

	x$setinverse(inv)

	inv
	
}
