## Put comments here that give an overall description of what your
## functions do

# The matrix inversion is a costly computation and there may be some benefits
# caching the inverse of a matrix rather than compute it repeatedly. 

# The following two functions help in creating a cache for the inverse of 
# a matrix.

# The first function, makeCacheMatrix creates a list containing the function to
#1.set values for the matrix
#2.get values of the matrix
#3.set values for the inverse of the matrix
#4.get values of the inverse of the matrix

##create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	
	 ##initialise the inverse property
    inv <- NULL

	 ##method to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

	 ##method to get the matrix and return it
    get <- function() {
	x
	}
	 
	 ##method to set the inverse of the matrix 
    setinverse <- function(inverse) {
    inv <<- inverse
    }
    
      ##method to get the inverse of the matrix
    getinverse <- function() {
	inv
	}
	
	 ##list of all the methods 
    list( set=set , get=get , setinverse=setinverse , getinverse=getinverse )

## Write a short comment describing this function

## Compute the inverse of the matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated then the 
## "cachesolve" should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
	
	#return the inverse of the matrix x
    inv <- x$getinverse()

	#if the value is already assigned, return the inv value
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }

	#get the matrix
    a <- x$get()

	#Compute the inverse using the Solve() function
    inv <- solve(a)

	#Set the inverse and return the value
    x$setinverse(inv)
    inv
}