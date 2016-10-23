
## makeCacheMatrix
#  SetMatrix   		 	set the value of a matrix
#  GetMatrix		  	get the value of a matrix
#  SetInverse      	set the cached value (inverse of the matrix)
#  GetInverse       get the cached value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL  							 
  
  # store the matrix 
  SetMatrix <- function(y) {					 
    x <<- y							 
    inv <<- NULL 					 
  }
  
  # return the stored matrix
  GetMatrix <- function() {
    x
  }									
  
  # set the inverse of matrix
  SetInverse <- function(inverse) {
    inv <<- inverse
  }										
  
  # ge the cache matrix
  GetInverse <- function() {
    inv
  }										
  
  # return a list. Each named element of the list is a function
  list(SetMatrix = SetMatrix, GetMatrix = GetMatrix, SetInverse = SetInverse, GetInverse = GetInverse)  # return a list. Each named element of the list is a function
  
  
}

##The following function calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and sets the value of the matrix in the cache via the SetInverse function.


cacheSolve <- function(x, ...) {

    # get the cached value						
  inv <- x$GetInverse() 	
  
   # skips the computation if inv already existed
  
  if (!is.null(inv)) {					    
    message("getting cached data")       
    return(inv)
  }											
  
  # If not calculate the inverse with solve () and assign the result to Inv.
  matrix <- x$GetMatrix()								
  inv <- solve(matrix, ...) 					
  x$SetInverse(inv)	
  
  # return the inverse		
  inv			
}
