## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y){
      x <<- y
      inverse <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    
    list(set = set,get = get,setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x = makeCacheMatrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  
   inv <- x$getinverse()
 
   if(!is.null(inv)){
     message("not calculating inverse, fetching from cache")
     return (inv)
   }
   
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinv(inv)
  inv

}
