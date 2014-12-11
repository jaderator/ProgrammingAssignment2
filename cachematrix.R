## Function to compute the inverse of a matrix and then store it as a cached variable
## This enables quicker returns on the means if it is needed in later code.

## makeCacheMatrix function creates a matrix, sets the value, sets the inverse, and returns the inverse.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL					## sets m to NULL 
	setmatrix <- function(y)  {	## sets the value of the matrix
		x <<- y					## caches the matrix in y 
		m <<- NULL
	}
	getmatrix <- function() x
	setinverse <- function(inverse) m <<- inverse ##
	getinverse <- function() m
	list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
								## sets the list of the created functions
}


## cacheSolve function works to see if the inverse has already been cached and returns if it has.
## If not, the inverse is calculated and then stored in the cacheSolve variable.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse() 	#checks to see if the inverse is already calculated
	if(!is.null(m)) {		#checks to see if the inverse (m) has a value and proceeds if its not empty
		if(x$setmatrix() == x$getmatrix()) {#checks to see if the matrix has changed and proceeds if it hasn't
			message("getting cached data") 	#prints a message to let user know the value is getting retrieved
			return(m) 						#prints the cached data
		}
	}
	dat <- x$getmatrix()	 
	m <- solve(dat, ...)  	## computes the inverse
	x$setinverse(m)			## runs the set inverse
	m						## returns the inverse
}
