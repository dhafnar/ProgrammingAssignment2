## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. If 
## the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}

# Create the matrix
someMat <- matrix(c(5,3,1,1,-6,2,3,8,4,2,1,3,3,1,2,3),4,4)

# Check the matrix
someMat

# Assign result of function to cacheMat
cacheMat <- makeCacheMatrix(someMat)

# Call the function
cacheSolve(cacheMat)

# Check against solve() 
solve(someMat)
