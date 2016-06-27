#---------------------------------------------------------------
# Coursera : R programming, week 3 : Peer Graded Assignment : 
#    Programming Assignment 2: Lexical Scoping
#---------------------------------------------------------------

#Write the following functions:
#makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
       inv <- NULL                            # initializes to NULL
       set <- function(y) {                   # creates the matrix
             x <<- y 
             inv <<- NULL 
         } 
       get <- function() x                               # gets the value of the matrix
       setinverse <- function(inverse) inv <<- inverse   # inverts the matrix
       getinverse <- function() inv                      # gets the inverted matrix 
       list(set=set, get=get,                            # returns the created functions 
            getinverse=getinverse) 
   } 

#----------------------
#cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
  if(!is.null(inv)) {                  # Test if the inverse has already been calculated
    message("getting cached data")   # If so get the cached data and skip computation
    return(inv)
  }
  data <- x$get()                    # Otherwise calculates data
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}





