## The functions makeCacheMatrix() and cacheSolve() creates a special "matrix" object, it calculates and caches its inverse. It calculates the inverse for new matrix values, if the inverse already exist then it retrieves the inverse value from cache.

## makeCacheMatrix function takes 'matrix' object as parameter and returns a list of functions to set values to a matrix , get the value of matrix , set the inverse of matrix and get the inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {

  inv<-NULL
  set_matrix <- function(y){
    x<<-y
    inv<<-NULL
  }
  get_matrix <- function() x
  set_inv <- function(solve) inv<<-solve
  get_inv <- function() inv
  list(set_matrix=set_matrix, get_matrix=get_matrix, set_inv=set_inv, get_inv=get_inv)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve  retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

inv<-x$get_inv()
if (!is.null(inv)){
  return(inv)
}
data<-x$get_matrix()
inv<-solve(data,...)
x$set_inv(inv)
inv
          
}
