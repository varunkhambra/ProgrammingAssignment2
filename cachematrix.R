##THe following functions, makeCacheMatirx and cacheSolve cache the inverse of a matrix.

## creates a matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize
        inverse <- NULL
                
        set <- function( matrix ) {
                x <<- matrix
                inverse <<- NULL
        }        
        
        get <- function() x
        
         ## to set the inverse of the matrix
        setInverse <- function(y) {
                inverse <<- y
        }
        
        ##to get the inverse of the matrix
        getInverse <- function() inverse   
        
        ## Return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## computes the inverse of the matrix returned by the makeCacheMatrix()
##if inverse is already calculated then and input matrix is same then
##cacheSolve will retrieve the inverse from cache
cacheSolve <- function(x, ...) {
      
        
        inverse <- x$getInverse()
        
        ## return the inverse if its already set
        if( !is.null(inverse) ) {
                message("getting cached data")
                return(inverse)
        }
        
        ## Get the matrix
        data <- x$get()
        
       ##solve inverse
        inverse <- solve(data) ##%*% data
        
       ## Set the inverse
        x$setInverse(inverse)
        ## Return
        inverse
}


## For output i ran the below mentioned.
##1.cacheSolve(makeCacheMatrix(x))
##2.mm<-makeCacheMatrix(x)
## mm$getInverse() %*% cacheSolve(mm)
