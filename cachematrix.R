## Put comments here that give an overall description of what your
## functions do

## First function mirrors what the example did with a vector, and needed very little
## alteration to complete. In essence, it creates a list of named functions to set &
## get a matrix within the function, and set and get the inverse of that matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,               #gives the name 'set' to the set() function defined above
       get = get,               #gives the name 'get' to the get() function defined above
       setinverse = setinverse, #gives the name 'setinverse'...
       getinverse = getinverse) #gives the name 'getinverse'...
}


## This function calls the named functions in makeCacheMatrix to 1- check if there is
## a cached inverse already, 2- solve for the inverse if there is no cache, and 3- cache
## the inverse of that matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
