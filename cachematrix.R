## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). 
## This function creates a special "matrix" object that can 
## cache its inverse

## makeCacheMatrix creates a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of inverse of the matrix
#4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y) {
    x<<-y
    inversermatrix<<- NULL
  }
  get<- function() x
  setinverse<- function(inverse) inversematrix<<-inverse
  getinverse <- function() inversematrix
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
#The following function returns the inverse of the matrix. 
#It first checks if the inverse has already been computed.
#If so, it gets the results and skips computation. 
#If not, it computes inverse. and sets the value in the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversematrix<-x$getinverse()
  if(!is.null(inversematrix)) {
    message("Getting data from cache")
    return(inversematrix)
  }
  data<-x$get()
  inversematrix<-solve(data)
  x$setinverse(inversematrix)
  return(inversematrix)
}
