

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
                
        set <- function( matrix ) {
                x <<- matrix
                inverse <<- NULL
        }        
        
        get <- function() x
        
        
        setInverse <- function(y) {
                inverse <<- y
        }
        
        
        getInverse <- function() inverse        
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}



cacheSolve <- function(x, ...) {
      
        
        inverse <- x$getInverse()
        
        
        if( !is.null(inverse) ) {
                message("getting cached data")
                return(inverse)
        }
        
        
        data <- x$get()
        
       
        inverse <- solve(data) ##%*% data
        
       
        x$setInverse(inverse)
        
        inverse
}


## For output i ran the below mentioned.
##1.cacheSolve(makeCacheMatrix(x))
##2.mm<-makeCacheMatrix(x)
## mm$getInverse() %*% cacheSolve(mm)
