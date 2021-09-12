#The function, makeCacheMatrix produce a matrix and can set and get the
#value of the matrix and set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This function works with the inverse matrix returned from makeCacheMatrix. 
#Than cacheSolve should get the cached inverse matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

##to test the function use a matrix which is invertible like a
a <- matrix(c(2,1,1,2),2,2)
print(a)

a_inv <- makeCacheMatrix(a)
cacheSolve(a_inv)

##getting cached inverse data
cacheSolve(a_inv)
