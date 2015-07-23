## makeCacheMatrix is a function which stores an invertible matrix
## and cacheSolve is a function which computes the inverse of this matrix.

## makeCacheMatrix creates a list of four functions which sets a matrix,
## gets the value of that matrix, sets the inverse of a matrix and gets 
## the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <- inverse  
  
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve () computes the inverse of the matrix stored in makeCacheMatrix.

cacheSolve <- function(x = matrix(), ...) 
{
  inv <- x$getinverse()
  
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data,...)
  
  x$setinverse(inv)
  
  inv
  ## Return a matrix that is the inverse of 'x'
}
