## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {   #X is defined by the value of the matrix##
  inv <- NULL            ## We asume that the initial value of the matrix is null##
  set <- function(y) {    ##The value of the matrix its going to be give by this function##
    x <<- y ## It is use to modify variables in parent levels## 
    inv <<- NULL
  }
  get <- function() x  #The value of the matrix will be given outside the set function##
  setInverse <- function(inverse) inv <<- inverse  #The value of inv will be given by the inverse of the matarix##
  getInverse <- function() inv    ##This funciont obtains the inverse of the matrix##  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()     ##Obtains the inverse and assing the result to the element inv##
    if (!is.null(inv)) {
      message("getting cached data")   #If the inverse is already calculated it would avoid de computation and would return this message##
      return(inv)  #on the contrary it would have to make the computation and return the inverse##
    }
    mat <- x$get()
    inv <- solve(mat, ...) #Solve is the generic function to get an inverse of a matrix##
    x$setInverse(inv)  #You have to use the function set inverse to get the inverse of a matrix##
    inv
  }
  
  
}
