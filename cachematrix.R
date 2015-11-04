# makeCacheMatrix creates the list to contain a function for 
# 1. setting the value of the matrix 
# 2. getting the value of the matrix 
# 3. setting the value of inverse of the matrix 
# 4. getting the value of inverse of the matrix 

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
 
# The function below returns the inverse of the matrix. First it checks if 
# the inverse had already computed. If so, gets the result and skips computing.
# If not, computes the inverse and sets the value in the cache using 
# setinverse function. 
 
 
# Below assumes the matrix is always invertible. 

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