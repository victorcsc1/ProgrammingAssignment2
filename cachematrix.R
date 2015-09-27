
mCacheTrix <- function(x =matrix()) {
  #list containing functions to 
  ## set the matrix
  ##get matrrix
  ##set matverse
  ##get matverse
  
  matverse = NUll
  set = function(y){
    x <<- y
    matverse <<- NULL
  
  }
  get = function() x
  setvers = function(inverse) matverse <<- inverse
  getvers = function() matverse
  list(set=set, get=get, setvers=setsetvers, getvers=getvers)
}

solvemat <- function(x, ...) {
  matverse = x$getvers
  
  if(!is.null(matverse)){
    #load from first input
    message("getting cached data")
    return(matverse) #if it's empty
  }
    data.mat = x$get()
    matverse = solve(data.mat, ...)
  #Find it's inverse
  #Set new value by setmatverse FUN
  x$setverse(matverse)
  
  return(matverse)
   
}
