## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.



makeCacheMatrix <- function(x = matrix()) 
{
  i = NULL
  set = function(x1) 
  {
    x <<- x1
    inv <<- NULL
  }
  get = function() x
  seti = function(inverse) i <<- inverse 
  geti = function() i
  list(set=set, get=get, seti=seti, geti=geti)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cach


cacheSolve <- function(x, ...) 
{
  i = x$geti()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  m = x$get()
  i = solve(m, ...)
  x$seti(i)
  
  return(i)
}

}
