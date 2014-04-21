## Since computing a matrix inverse can take time, we may want to save an inverse to the 
## cache once it is computed. makeCacheMatrix creates a matrix "object", and cacheSolve 
## checks to see if an inverse has already been calculated. 
## If not, the function will calculate the inverse and save it to the cache.

## The makeCacheMatrix function creates a matrix "object" with methods that allow the user to 
## get or set the inverse of the matrix, as well as get or set the matrix itself. 

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL

set <-function(y){
	x <<- y 
	inv <<- NULL
	}

get <- function() x 

setinverse <-function(inverse) inv <<- inverse 

getinverse <-function() inv

list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function checks to see if the matrix inverse has been cached. 
## If not, the inverse is calculated and set using $setinverse()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
		
		if(!is.null(inv)){
			message("Getting cached inverse")
			return(inv)
		}
		
		x.matrix <- x$get()
		inv <- solve(x.matrix)
		x$setinverse(inv)
		inv
}
