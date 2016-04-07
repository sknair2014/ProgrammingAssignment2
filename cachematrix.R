##     makeCacheMatrix: This function will create a matrix object that can cache its inverse.
##     cacheSolve: This function computes the inverse of the matrix returned by makeCacheMatrix above.
##     If the inverse has already been calculated (and the matrix has not changed),
##     then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix function take a matrix x as input and cache its inverse using solve function.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
