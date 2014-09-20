## The functions makeCacheMatrix and cacheSolve may be used to calculate the inverse matrix
## of a given matrix M. The second time the inverse of a given 'cacheMatrix' cM=makeCacheMatrix(M)
## is needed the result of the first computation is reused (loaded from cache).



# The function makeCacheMatrix creates an object (a 'cacheMatrix') based on a given matrix M.
# The result of this function is a list needed as input for the cacheSolve function.

makeCacheMatrix <- function(M = matrix()) {
  I <- NULL
  set <- function(y){
    M <<- y
    I <<- NULL 
  }
  get <- function() M
  setinv <- function(inv) I <<- inv
  getinv <- function() I
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# cacheSolve computes the inverse of a given 'cacheMatrix' cM created by the function
# makeCacheMatrix(). It returns the inverse Matrix of M, where cM=makeCacheMatrix(M).

cacheSolve <- function(cM, ...) {
## Return a matrix that is the inverse of cM=makeCacheMatrix(M)
  I <- cM$getinv()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data <- cM$get()
  I <- solve(data, ...)
  cM$setinv(I)
  I
}
