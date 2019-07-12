## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv<- NULL                     ## inv will hold the value of inverse of matrix 
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function()x
  setinverse <- function(inverse)
    inv<<-inverse
  getinverse <- function()inv
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function
## If the inverse has already been calculated then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
      inv<-x$getinverse()
      if(!is.null(inv)){
        message("Getting data that is Cached")
        return(inv)
      }
      mat <- x$get()
      inv <- solve(mat,...)
      x$setinverse(inv)
      inv
        ## Return a matrix that is the inverse of 'x'
}
## sample output
##  > z <- matrix(5:8,2,2)
##  > z
##        [,1] [,2]
##   [1,]    5    7
##   [2,]    6    8
##  > CacheMatrix<-makeCacheMatrix(z)
##  > CacheMatrix$getinverse()
##  NULL
##  > cacheSolve(CacheMatrix)
##  [,1] [,2]
##  [1,]   -4  3.5
##  [2,]    3 -2.5
