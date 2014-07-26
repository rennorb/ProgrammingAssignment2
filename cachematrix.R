## Two functions makeCacheMatrix and cacheSolve are provided 
## to cache the inverse of a matrix 

## makeCacheMatrix returns a list functions providing get and 
## set function for a given matrix x and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv  
  getinverse <- function() inverse
  
  list(set=set,get=get,
       setinverse=setinverse, getinverse = getinverse)
  
}


## cacheSolve returns the inverse of the matrix x.
## In case the inverse has already been calculated it retrieves
## the inverse from the cache using the makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m_inv <- x$getinverse()
  if (!is.null(m_inv)){
    message("getting cached data")  
    return(m_inv)
  }
  m <- x$get()
  m_inv <- solve(m)
  x$setinverse(m_inv)
  return(m_inv)    
}
