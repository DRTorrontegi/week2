## The functions create a special matrix that can keep 
## cache of the matrices data and its inverse

## makeCacheMatrix creates that "matrix" which is really a list
## containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ### set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ### get the value of the matrix
  get <- function() x
  
  ### set the value of the inverse of the matrix
  setsolve <- function(solve) m <<- solve
  
  ### get the value of the inverse of the matrix
  getsolve <- function() m
  
  # return the list
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve() # retrieve from cache
  
  if(!is.null(m)) { # we got cached data
    message("getting cached data")
    return(m)
  }
  
  # cache isn't filled yet
  data <- x$get()       # get it
  m <- solve(data, ...) # solve it
  x$setsolve(m)	      # set it
  m		      # return it
}
