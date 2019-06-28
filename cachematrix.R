# makeCacheMatrix creates a list containing a function to 
# set the value of the matrix and get the value of the matrix, as well as
# set the value of inverse and get the value of inverse 
makeCacheMatrix <- function(x = matrix()) { 
     inv <- NULL 
     set <- function(y) { 
         x <<- y 
         inv <<- NULL 
     } 
     get <- function() x 
     setinverse <- function(inverse) inv <<- inverse 
     getinverse <- function() inv 
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 

# cacheSolve returns the inverse of the matrix. It checks if the inverse 
# has been computed and if it has, it returns the result and skips
# computation. If not, it computes the inverse and sets the value in the 
# cache with setinverse. 

cacheSolve <- function(x, ...) { 
    inv <- x$getinverse() 
    if(!is.null(inv)) { 
          message("getting cached data.") 
          return(inv) 
      } 
    data <- x$get() 
   inv <- solve(data) 
   x$setinverse(inv) 
   inv 
} 