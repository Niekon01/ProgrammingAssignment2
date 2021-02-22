## This R files aims at trying to cache the inverse of a matrix to reduce the number of times it is computed

## This function enables the matrix object's inverse to be cached

makeCacheMatrix <- function(x = matrix()) {
  	invr <- NULL ## invr will save the inverse of the matrix
  		set <- function(y){ ## set function is defined and assigned to new
  		x <<- y ## the value of matrix originally
  		invr <<- NULL ## if a new matrix is found, revert invr to NULL
  	}

  	get <- function() x ## returns value of the matrix in argument
  	setInverse <- function(inverse) invr <<- inverse ## assigns value of invr in orignal(parent) environment
  	getInverse <- function() invr ## gets the value of invr where called
  	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## required inorder to call the function using $
}


## This function computes the inverse of the "matrix" returned by the above function
## In case the inverse has already been calculated, and the matrix is same, cacheSolve will retieve the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invr <- x$getInverse()
  	if(!is.null(invr)){
  		message("getting cached data")
  		return(invr)
  	}
  	mat <- x$get()
  	invr <- solve(mat,...)
  	x$setInverse(invr)
  	invr
}
