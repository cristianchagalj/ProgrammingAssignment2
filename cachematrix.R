## This function creates an object "matrix":
makeCacheMatrix <- function(x = matrix()) {
  
  # create a null object
  m <- NULL
  
  # this function set the value of matrix "x"
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  
  # this function return the matrix "x"
  get <- function() x
  
  # this function set the matrix "m" (inverse)
  setinv <- function(solve) m <<- solve 
  
  # this function return the matrix "m" (inverse)
  getinv <- function() m
  
  # list the methods of the object matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
} ## end of the first function


## This function returns a matrix that is the inverse of "x":
cacheSolve <- function(x, ...) {
  
  # call the function "getinv"
  m <- x$getinv() 
  
  # if the matrix inverse "m" is not null, so return "m":
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
 
  # else the matrix inverse "m" is null, so calculate:
  
  # 1) cache the matrix "x" (call the property "get" of the matrix)
  data <- x$get() 
  
  # 2) calculate the inverse of matrix "x" ("m") using the function solve()
  m <- solve(data, ...)
  
  # 3) set to this property the value of "m"
  x$setinv(m) 
  
  # 4) return the matrix "m"
  m 
  
} ## end of the second function
