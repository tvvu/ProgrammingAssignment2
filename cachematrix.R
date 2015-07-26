#Overal description: 
#First function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.
#Second function (cacheSolve) computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

#The first function, makeCacheMatrix stores a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse


makeCacheMatrix <- function(x = matrix()) { #the main function that stores a list of functions
      m <- NULL 
      set <- function(y) { 
        x <<- y #change the vector stored in the main function. We need to use this later so it has to be stored in an environment that is different from the current environment
        m <<- NULL #restores the null value of m, because the old value is not needed anymore, the new value needs to be calculated through the function cacheSolve
      }
      get <- function() x #restores the matrix x stored in the main function
      setinverse <- function(solve) m <<- solve #stores the value of the input in the variable m into the main function
      getinverse <- function() m #returns the value of the input in the variable m into the main function
      list(set = set, get = get, #store the 4 functions with the respective names
        setinverse = setinverse,
        getinverse = getinverse)
}
  

#The second function subsets the main function, i.e. to take out a function stored in the main function

cacheSolve <- function(x, ...) {
      m <- x$getinverse() #verifies if the value m, stored previously with getinverse, exits and if not NULL
      if(!is.null(m)) { #if it already exists, simply returns a message and the value m
      message("getting cached data")
      return(m)
      }
      data <- x$get() #get the vector stored with makeCacheMatrix
      m <- solve(data, ...) #calculates the inverse matrix
      x$setinverse(m) #stores it in the object generated assigned with makeCacheMatrix
      m
}
