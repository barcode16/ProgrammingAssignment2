## The R code for this assignment has largely been adapted from the example code provided by 
## Roger Peng in the assignment example. Reference to "mean" has been changed to "inverse",
## and the "m" variable has been changed to "i" for (inverse"). These are the only code changes
## required for this assignment. The main element of the assignment other than these changes
## relate to the need to add explanatory comments.


## the makeCacheMatrix function creates a special "matrix" 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
## get the value of the matrix
  
  get <- function() x

## set the inverse of the matrix

  setinverse <- function(inverse) i <<- inverse

## get the inverse of the matrix

  getinverse <- function() i
  list( set = set, get = get, setinverse=setinverse, 
        getinverse=getinverse)
}


## The cachesolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  
## check if the inverse of the matrix has already been calculated
  
  if (!is.null(i))
  {
    message("getting cached data")
    return (i)
  }

## if the inverse of the matrix not already calculated, get the inverse of the matrix

  data <- x$get()
  i<-solve(data, ...)

## set the inverse of the matrix

  x$setinverse(i)
  i
}
