## This function return a list with the following methods for a matrix.
## set:  set the value of the matrix 
## get:  get the value of the matrix
## setinverse:  set inverse of the matrix
## getinverse:  return the inverse of the matrix
makeCacheMatrix <- function(m = matrix()) {
  
  m_inverse <- NULL
  
  ## set the matrix value and the inverse of the matrix to NULL
  set <- function(y){
    m <<- y
    m_inverse <<- NULL
  }
  
  ## return the matrix value
  get <- function() m
  
  ## set the inverse of the matrix
  setinverse <- function(inverse) m_inverse <<- inverse
  
  ## return the inverse of the matrix
  getinverse <- function() m_inverse
  
  list (set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function returns the inverse of the input matrix
cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'm'
  m_inverse <- m$getinverse()
  if(!is.null(m_inverse)) {
    message("getting cached data")
    return(m_inverse)
  }
  data <- m$get()
  m_inverse <- solve(data, ...)
  m$setinverse(m_inverse)
  m_inverse
}