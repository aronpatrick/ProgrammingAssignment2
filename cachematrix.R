
## Inverting matrices can be computationally time-consuming. In order to reduce computing time, 
## if the inverted matrix is stored in the cache, so that if already solved for, it can be retrieved rather than 


## The purpose of this function is twofold.
## First, to calculate the inverse of the matrix passed to it.
## Second, if the inverse of the matrix has already been calcuated, it is retrieved from the cache.

makeCacheMatrix <- function(x = matrix()) {
   inv_x <- NULL
   set <- function(y) {
     x <<- y
     inv_x <<- NULL
   }
   get <- function() x
   setinverse<- function(inverse) inv_x <<-inverse
   getinverse <- function() inv_x
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
  
}


## This function below, cacheSolve, returns the matrix created above if available. If not it solves it. 

cacheSolve <- function(x, ...) {

   inv_x <- x$getinverse()
   if (!is.null(inv_x)) {
     return(inv_x)
   } else {
     inv_x <- solve(x$get())
     x$setinverse(inv_x)
     return(inv_x)
   }
}
