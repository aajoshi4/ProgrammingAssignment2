## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	## Inverse is initially Null variable	
        I <- NULL
	
	## set function stores the matrix in cache
        set <- function(y) {
                x <<- y
                I <<- NULL
        }

	## get function retrives the matrix from cache
        get <- function() x

	## setinv is a function that evaluate inverse & store it in cache 
        setinv <- function(solve) I <<- solve

	## getinv is a function that retrives inverse stored in the cache
        getinv <- function() I

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

	## First find if inverse exists in cache
	I <- x$getinv()

	## if inverse exists in cache retrive it from memory	
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }

	## if not already in cache store in cache 		
        data <- x$get()
        I <- solve(data, ...)
        x$setinv(I)
        I

}
