makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #set the value of the vector
  set <- function(y){ 
    x <<- y
    m <<- NULL
  }
  
  #get the value of the vector
  get <- function() x 
  #set the value of the inverse
  setsolve <- function(solve) m <<- solve
  #get the value of the inverse
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x = matrix(), ...) {
  m <- x$getsolve()
  
  #checks to see if the inverse has already been calculated
  #if not null, gets inverse from the cache and skips the computation
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  #if null, calculates inverse of the data 
  data <- x$get()
  m <- solve(data, ...)
  
  #sets the value of the inverse
  x$setsolve(m)
  m
}
