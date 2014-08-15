
  ## [create a special object that stores a matrix]
  
  ## It's a list with 4 elements
  ## set:set the value of the matrix
  ##get: get the value of the matrix
  ##setinverse: set the value of the inverse of the matrix
  ##getinverse: get the value of the the matrix
  
  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
  
  }


  ## Cache the value of the inverse of the matrix
  ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
