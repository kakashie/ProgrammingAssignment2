## The following two functions (makeCacheMatrix and cacheSolve) enable
## the implementation of caching matrix inverse computations. Matrix inverse
## computation can be a time consuming task especially for large matrices,
## therefore this implementation helps save time in computation. If a matrix
## inverse has already been calculated, the answer is retrieved when the
## same computation is encountered again


## The following function returns a list which contains four different functions
## for storing(setting) the matrix and its inverse, retrieving(getting) the matrix
## and its inverse

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    set <- function(mat) {
           
           x <<- mat
           inv <<- NULL
           
    }
    
    get <- function() x
    
    setinv <- function(mat_inv) inv <<- mat_inv
    
    getinv <- function() inv
    
    list ( set = set, get = get,
           setinv = setinv, getinv = getinv)
}


## The function below calculates the inverse of a matrix passed
## to it by the makeCacheMatrix function. It checks, before
## calculation if the inverse has already been found. If the
## inverse is cached already, it retrieves and returns the 
## stored inverse. If no inverse is stored, it calculates the
## inverse, caches it and returns the final answer.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        if(!is.null(inv)) {
          
          message("getting cached data")
          
          return(inv)
        }
        
        inmatrix <- x$get()
        
        inv <- solve(inmatrix, ...)
        
        x$setinv(inv)
        
        inv
}
