## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  setmatrix<-function(y) ## function assigns y matrix to x
  {
    x<<-y
    inv<<-NULL
  }
  getmatrix<-function() x  ## function returns matrix in cache memory
  setinverse<-function(inverse) inv<-inverse ## function assigns inverse matrix 
  getinverse<-function() inv ##function returns inverse matrix from cache memory
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse
  if(!is.null(inv)) ## Checks if inverse has already been calculated or not
  {
    print("getting cache data")
    return(inv) ## Returns cache memory data if calculated
  }
  matrix<-x$getmatrix 
  inverse<-solve(matrix) ##Calculates inverse of matrix after getting data from cache memory
  x$setinverse(inverse)  ##Assigns inverse matrix
  inverse
}

