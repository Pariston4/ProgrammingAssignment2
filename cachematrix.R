


#A pair of functions that set the inverse of a matrix

#Set a matrix that can cache the inverse
makeCacheMatrix <- function(m = matrix()) {
      
#Initiate the inverser property
      inv <- NULL
#Set the matrix
      set <- function(m) {
            m <<- matrix
            inv <<- NULL
      }
#Get the matrix
      get <- function(){
#Return m
            m
      }
#Set the inverse function
      setsolve <- function(solve)inv 
      inv <<- solve
      
#Get the inverse
      getsolve <- function()inv 
#Return the inverse property
            inv
      
#Get the list
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
      inv <- x$getsolve()
      if(!is.null(inv)) {
            message("getting inversed matrix")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setsolve(inv)
      inv
}

m <- matrix(1:4, 2, 2)
m1 <- makeCacheMatrix (m)
cacheSolve(m1)
