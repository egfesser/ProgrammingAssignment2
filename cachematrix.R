## Creates a special "matrix" object that can cache its inverse. Note that this
## function needs to be used to create the object which is used as input to
## the cacheSolve function, since a list is needed rather than a simple matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## For this assignment, assume that the matrix supplied is always 
        ## invertible.

        xInverse <- NULL
        
        set <- function(y) {
                x <<- y
                xInverse <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setInverse <- function(inverse) {
                xInverse <<- inverse
        }
        
        getInverse <- function() {
                xInverse
        }
        
        list(set = set, 
             get = get,
             setInverse = setInverse, 
             getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix. If
## the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.
  
        xInverse <- x$getInverse()
        
        if(!is.null(xInverse)) {
          message("getting cached data")
          return(xInverse)
        }
        
        data <- x$get()
        xInverse <- solve(data)
        x$setInverse(xInverse)
        xInverse      
}
