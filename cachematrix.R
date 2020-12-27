## Put comments here that give an overall description of what your
## functions do

## this function creates a matrix and then caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  get<- function()x 
  setinv<- function(inverse) inv<<- inverse
  getinv<- function() inv
  list(set= set, get= get,
       setinv= setinv, getinv= getinv)
}


## the function below calculates the inverse of a matrix
##it assumes that all the matrixes that it receives are inversible
##first it checks if there is an inverse of a matrix in cache
##if not it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv<- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat<- x$get()
  inv<- solve(mat,...)
  x$setinv(inv)
  inv
  
   ## Return a matrix that is the inverse of 'x'
}
