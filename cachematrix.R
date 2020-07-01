## cmakeCacheMatrix -- creates a method with the below methods:
## set --  updates the values of the matrix
## get --  retrieves  the value of the matrix
## setInverse  -- updates inverse matrix
## getInverse -- retrieves inverse matrix

## The function makeCacheMatrix creates a matrix with methods to that enable it to 
## retrieve and set its values and to retrieve and set ins inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setInverse <-  function(solveMatrix) i <<- solveMatrix
    getInverse <- function() i
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## The cacheSolve function converts the inverse of the matrix created by makeCacheMatrix function
## If the inverse matrix already exist and has not changed it will return it from cache. If it
##  does not exist it will compute and cache the inverse matrix

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
    i <- x$getInverse()
    
    if(!is.null(i)){
         message ("getting cached data")
         return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
    
}
