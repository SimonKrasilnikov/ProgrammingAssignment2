## Function makeCacheMatrix creates and manages cache matrix
## Creates a list of:
##   set -- setup the values of the cache matrix
##   get -- returns cached values if presented 
##   setInverse -- setup the value of the inverse matrix
##   getInverse -- gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## define the inverse matrix as NULL
   ## set -- setup the values of the cache matrix 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
   ##   get -- receives cached value of matrix
    get <- function() x
   ##   setinverse -- calculates the inverse matrix and setup the value of the inverse matrix in cache   
    setInverse<- function(solve) inv <<-solve
   ## getinverse -- gets the value of the inverse matrix
    getInverse <- function() inv
   ## Create a list containing functions to set, get, setInverse and getInverse   
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Fucnction calculates inverse of matrix or returns cached value if exists

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- makeCacheMatrix(x)$getInverse
    ## get cached matrix
    data <- makeCacheMatrix(x)$get
    
    ## if cache of inverted matrix exists and matrix hasn't changed
    ## then return its value
    
    if((!is.null(invMatrix)) && (data==x)) {
        message("getting cached data")
        return(invMatrix)
    }
    ## Calculate inverse matrix
    invMatrix <- solve(data, ...)
    ## Cache the calculated values
    makeCacheMatrix(x)$setInverse(invMatrix)
    ## Return inversion matrix
    invMatrix
}
