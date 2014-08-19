
## This function creates a special vector of functions
## for setting and getting inverse of matrix x (calculated by cacheSolve)
## and for setting and getting value of x
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inv) inverse <<- inv
  get_inverse <- function() inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This funcion calculates of inverse of matrix contained in a special vector 
## provided by makeCacheMatrix function. After first calculation it stores calculated
## value of inverse of matrix in a provided vector. 
## Returns inverse of matrix contained in vector x.

cacheSolve <- function(x) {
  
  ## trying to get cache value
  inverse <- x$get_inverse()
  
  ## returns if cached value is not null
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  #otherwise getting data of matrix
  data <- x$get()
  #and calculating inverse of it
  inverse <- solve(data)
  #setting inverse for later usage
  x$set_inverse(inverse)
  #returning calculated inverse
  inverse
}

 ## Example 
 m <- matrix(c(1,2,3,0,1,4,5,6,0),ncol=3,nrow=3)
 x <- makeCacheMatrix(m)
 inverse_of_m <- cacheSolve(x)
