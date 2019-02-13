# makeCacheMatrix object will has four functions contained in a list.
# 1. "set" the matrix
# 2. "get" the above matrix
# 3. "set" the inverse values of the matrix
# 4. "get" the inverse values of the above matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function below returns the inverse of the matrix.
## It checks if the same matrix has been computed already. If so, a message is displayed.
## If not, the inverse is computed and the value gets stored in the cache
cacheSolve <- function(x, ...) {
  #get inverse
  i <- x$getInverse()
  #check if already in the cache. If so, output message and inverse.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #if not in cache
  data <- x$get()
  i <- solve(data, ...) #Use "solve" function for inverse.
  x$setInverse(i)
  i
}
