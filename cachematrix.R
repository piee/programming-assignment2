## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## stores the value, first value NULL
  cache<-NULL
  ## create the matrix
set<-function(y){
  x<<-y
  cache<<-NULL
}
## get the value of the matrix
get<-function()x
## inversed matrix, stored in cache
setMatrix<-function(inverse)cache<<-inverse
## get the inversed matrix from cache
getInverse<-function()cache
## show the function
list(set=set,get=get,setMatrix=setMatrix,getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cache<-x$genInverse()
  ## if the null is not in cache, it shows matrix 
  if(!is NULL(cache){
    message("getting cached data")
    return(cache)
  }
  ## generate matrix
  matrix<-x$get()
  ## matrix is square and invertible, if it is not show differently
  tryCatch({
    cache<-solve(matrix,...)
  }, error=function(e){
    message("Error:")
    message(e)
    return(NA)
  }, warning=function(e){
    message("Warning")
    message(e)
    return(NA)},finally={x$setMatrix(cache)})
  ## show the result in the console 
  return(cache)
  }
