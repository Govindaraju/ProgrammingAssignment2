## Please follow the below steps to test the functions defined below
## mat <- matrix(1:4,2,2)
## cacheasblemat <- makeCacheMatrix(mat)
## inv1 <- cacheSolve(cachablemat)
## in the above step the inverse will be calculated and returned.
## inv2 <- cacheSolve(cachablemat)
## in the above step the inverse wont be calculated rather will be returned from the cache
## 

## makeCacheMatrix takes a matrix and returns an object which has methods to get and set the matrix
## attribute and its inverse.

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


## cacheSolve - returns an inverse of a matrix present within x which is of type makeCacheMatrix
## cachesolve first checks if the inverse exist for a given matrix, if present it returns it from
## it from the cache if not it calculates inverse, sets it to x and then returns the calculated
## inverse of the matrix

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
