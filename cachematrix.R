## The makeCacheMatrix will create a cache matrix from the given input matrix 'x'


makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize the find_inverse variable with NULL to compute inverse in the later process          
  find_inverse <- NULL 
  
  set_matrix <- function(val) {
    x <<- val                           ## <<- being the superassignment operator
    find_inverse <<- NULL
  }
  
  get_matrix <- function(){
    x
  }
  
  set_inverse <- function(inverse){
    find_inverse <<- inverse
  }
  
  get_inverse <- function(){
    find_inverse
  }
  
  list(set_matrix = set_matrix, get_matrix = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse)
}


## The cacheSolve() will check first whether the inverse exists in the cache and then return it.
## else solve the inverse and then return it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  find_inverse <- x$get_inverse()
  
  if (!is.null(find_inverse)) {
    ## This condition will check first whether the inverse exists in the cache!
        
    message("This matrix is from the Cache!\n")
    return(find_inverse)
        
  }
  
  else {
        
  matrix_values <- x$get_matrix()
  find_inverse <- solve(matrix_values, ...)
  x$set_inverse(find_inverse)
  
  ##Return the inverse of the matrix using find_inverse
  find_inverse
  
  }
  
}
