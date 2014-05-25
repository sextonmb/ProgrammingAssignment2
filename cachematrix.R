## Function to calculate/cache the inverse of a matrix, modelled on the example function for calculating/caching the mean of a vector:

#makeVector <- function(x = numeric()) {
#        m <- NULL
#        set <- function(y) {
#                x <<- y
#                m <<- NULL
#        }
#        get <- function() x
#        setmean <- function(mean) m <<- mean
#        getmean <- function() m
#        list(set = set, get = get,
#             setmean = setmean,
#             getmean = getmean)
#}
#cachemean <- function(x, ...) {
#        m <- x$getmean()
#        if(!is.null(m)) {
#                message("getting cached data")
#               return(m)
#        }
#        data <- x$get()
#        m <- mean(data, ...)
#        x$setmean(m)
#        m
#}




## Similar structure to makeVector, ie: 
#makeCacheMatrix creates a special "vector", which is really a list containing a function to

#   set the value of the matrix
#   get the value of the matrix
#   set the value of the inverse, inv
#   get the value of the inverse, inv

makeCacheMatrix <- function(x = matrix()){

	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## Function to solve, modelled on cachemean()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	
	## if inverse exists, return cached value
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}

	data <- x$get()
	inv <- solve(data) #compute inverse using solve()
	x$setinverse(inv)
	inv
}
