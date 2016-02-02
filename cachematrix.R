## The first function creates a list of functions and stores the matrix input 
##the second one computes
## its inverse after first checking if is already available. 

## This function takes a matrix argument and returns a list of 
## functions which can either retreive the matrix, superassign 
## the variable inv, retreive the inverse, or set a new matrix to x
## using superassignment. 

makeCacheMatrix <- function(x = matrix()){
inv <- NULL
set <- function(y){
  x <<- y
  inv <<- NULL
  
}
get <- function() x 
setinv <- function(inverse) inv <<- inverse
getinv <- function() inv 
list(set=set, get=get, setinv = setinv, getinv=getinv)
}


## This function inverts its matrix argument 
## only if there is nothing stored in the variable inv. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("retreiving cached data...")
    return(inv)
    }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
