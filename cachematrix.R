## Coursera Programming Assignment #2.1: Caching the inverse of a matrix.
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # initially defining invMat object as NULL object.
  invMat<-NULL
  
  set<-function(y){
    x<<- y
    invMat<<-NULL
  }
  
  get<-function() x
  
  setInvmatrix<-function(solve) invMat<<- solve
  getInvmatrix<-function() invMat
  list(set=set, get=get,
       setInvmatrix=setInvmatrix,
       getInvmatrix=getInvmatrix)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMat<-x$getInvmatrix()
  
  # checking if inverse of matrix is NULL, if not null then retrive from cache.
  if(!is.null(invMat)){
    message("Getting inverse of matrix from cached data")
    return(invMat)
  }
  
  # if inverse of matrix is NULL, recompute the inverse of matrix using solve().
  matrix<-x$get()
  invMat<-solve(matrix, ...)
  x$setInvmatrix(invMat)
  invMat        
}
