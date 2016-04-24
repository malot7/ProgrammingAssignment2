## These two functions cache the inverse of a matrix X.
## Indeed, matrix inversion is costly computation. This is the reason why it could be interesting to cache the inverse of a matrix instead of its computation repeatedly.natives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix

## This function creates a list with four elements that are the set and get matrix and the set and get inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }



## This function calculates the inverse of the "special" matrix created thnaks to the previous function.
## R=The idea is that if the inverse has already been computed, the computation are not made a second time. 

cacheSolve <- function(x) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  if (det(data)!=0)
  {m <- solve(data)}
  else m<-c('no invertible')
  x$setinv(m)
  m
}
