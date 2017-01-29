## Put comments here that give an overall description of what your
## functions do

# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x  
  setInverse <- function(inverse) 
    inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
          
 #Example to check the functions
# use the set function to "put in" a new matrix 
 m1 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
 myMatrix_object$set(m1)
 # and obtain its matrix inverse by
   cacheSolve(myMatrix_object)
#       [,1] [,2]
#[1,]    3    7
#[2,]    1    5

 cacheSolve(myMatrix_object)
#getting cached data
#       [,1] [,2]
#[1,]    3    7
#[2,]    1    5
#check to see if the inverse matrix is true 
n1<-matrix(c(3, 1, 7, 5), nrow = 2, ncol = 2)
n1
#      [,1] [,2]
#[1,]    3    7
#[2,]    1    5
m1 %*% n1
#      [,1] [,2]
#[1,]    1    0
#[2,]    0    1         
