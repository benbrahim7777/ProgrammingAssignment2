## This function creates a special "matrix" object that can cache its inverse.
## 
## 1 - set the value of the vector
## 2 - get the value of the vector
## 3 - set the value of the inverse
## 4 - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseValue <- NULL
  
  ## 1 - set the value of the vector
  set <- function(y) {
    x <<- y
    inverseValue <<- NULL
  }
  ## 2 - get the value of the vector
  get <- function() x
  
  ## 3 - set the value of the inverse
  setinverse <- function(inverse) inverseValue <<- inverse  
  
  ## 4 - get the value of the inverse
  getinverse <- function() inverseValue   
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseValue <- x$getinverse()
  ## it first checks to see if the inverse has already been calculated. 
  ## If so, it gets the inverse from the cache and skips the computation. 
  if(!is.null(inverseValue)) {
    message("getting cached data")
    return(inverseValue)
  }
  data <- x$get()
  
  ## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
  ## in the cache via the setinverse function.inverseValue <- solve(data, ...)
  x$setinverse(inverseValue)
  inverseValue
  
}




