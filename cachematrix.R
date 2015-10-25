## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix creates custom matrix type capable of running four functions
#set stores the matrix in cache
#get recalls the matrix
#setinverse stores the inverse of original matrix
#get recalls the inverse of the original matrix
makeCacheMatrix <- function(x = matrix()) {			
		i <- NULL
		set <- function(y) { 					
			x <<- y
			i <<- NULL		#store matrix in cache
		}
		get <- function() x 					
		setinverse <- function(inverse) i <<- inverse   #set inverse matrix
		getinverse <- function() i     			#get inverse matrix
		list(set = set, get = get, 
			setinverse = setinverse, 
			getinverse = getinverse)			#creating list of functions

}


## Write a short comment describing this function
##cacheSolve take a custom matrix type created by the makeCacheMatrix function and calculates the inverse matrix of it.
#But first it checks to see if the calculation has been done before or not
#if it has been done then recalls the data from the cache. 
#if it has not done before then it calculates the inverse matrix then store it in the cache
cacheSolve <- function(x, ...) {					
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()	#get the x matrix's cache
	if(!is.null(i)) {		#checking whether the inverse is in cache or not
		message("getting cached data")	#send a notification indicating that this is in cache
		return(i)					#return the cache
	}
	data <- x$get()		#get the matrix used by makeCacheMatrix function
	i <- solve(data)		#calculate the inverse of the matrix
	x$setinverse(i)		#store the inverse matrix in cache using the makeCacheMatrix set function
	i				#returning the inverse matrix
}
