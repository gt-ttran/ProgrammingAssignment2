## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #initialize cached inverse to null
	cacheM <- NULL
  #sets x to new matrix y and resets cached inverse to null
	set <- function(y){
		x <<- y
		cacheM <<- NULL
	}
  #return the matrix
	get <- function() x
  #sets the cached inverse
	setInverse <- function(inv) cacheM <<- inv
  #gets the cached inverse
	getInverse <- function() cacheM
  #create a object with the four named elements
  list(set=set, get=get, setInverse = setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed, then the function retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #gets the cached inverse of x
	m <- x$getInverse()
  #if the cached inverse is not null then return the cached inverse
	if(!is.null(m)){
		message("getting cache inverse")
		return(m)
		}
  #otherwise, get the matrix
	data <- x$get()
  #and solve for the inverse
	m <-solve(data)
  #cache the inverse so that it can be accessed later
	x$setInverse(m)
  #return the inverse
	m
}
