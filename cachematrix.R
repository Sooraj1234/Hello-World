## This program makes use of two functions - 'makeCacheMatrix' and 'cacheSolve' that is used to calculate the inverse
## of a matrix that is provided as input, and store it in memory for later use. This reduces time for retrieval in 
## future much less. It also provides a perfec texample of lexical scoping in R

## The first function defined in the program is 'makeCacheMatrix' which is used to get the value of the input and its
## inverse, and also create a list that acts as a cache. 

makeCacheMatrix <- function(x = matrix())
{
  inv <- matrix()
  set <- function(y)
  {
    x <<- y
    inv <<- matrix()
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The second function 'cacheSolve' first checks if the input is already present in the cache. If yes, it prints out 
## the value of the inverse from it. If not, it proceeds to calculate the inverse and store it in the cache, while 
## also printing it out. 

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  n <- dim(inv)
  c = 0
  for (i in 1:n[1])
  {
    for (j in 1:n[2])
    {
      if (!is.na(inv[i,j]))
        c = c + 1
    }
  }
  if (c == n[1]*n[2])
  {
    message("getting cached inverse")
    return(inv)
  }
  new_data <- x$get()
  inv <- solve(new_data)
  x$setinv(inv)
  inv
}

