## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m_inv <- NULL
	set <- function(m) {
		x <<- m
		m_inv <<- NULL
	}
	get <- function() x
	
	setinverse <- function(inverse) m_inv <<- inverse
	getinverse <- function() m_inv
	list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve validates if the inverse of a matrix is cached in the system for a matrix value supplied. if it is available in cache, it would directly return the cache value else it processes the matrix and cache the inverse and returns the value
cacheSolve <- function(x, ...) {
        
		m_inv <- x$getinverse()
		if(!is.null(m_inv)){
			message("getting cached inverse matrix")
			return (m_inv)
		}
		data <- x$get()
		m_inv <- solve(data,...)
		x$setinverse(m_inv)
		m_inv ## Return a matrix that is the inverse of 'x'
}
