## Put comments here that give an overall description of what your
## functions do

## Creating matrix that can be cached in inverse

makeCacheMatrix <- function(x = matrix()) {
  inve = NULL # sets inverse as the null, holds matrix inverse values
  set <- function(y){
    x <<- y
    inve= NULL
  }
  get <<- function()x
  setinverse <<- function(inverse)inve <<- inverse
  getinverse <<- function() inve
  list(set=set, get=get,
      setinverse = setinverse
      getinverse = getinverse)
  
}


## Compute the inverse of of the matrix in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!isnull(inve)) {
    message("getting cached data")
    return(inve)
  }
  data <- x$get()
  inve <- inverse(data,...)
  x$setinverse(inve)
  inve
}
