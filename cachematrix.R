## These functions allow for caching of the potentially costly matrix inverse calculation


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The object returned will have the following methods available:
## - set(y): saves new matrix, clears cached inverse
## - get(): returns original matrix
## - setinverse(inverse): stores inverse
## - getinverse(): returns inverse of original matrix

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    # replace i with a new matrix, clear out cache
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # return original matrix
    get <- function() x
    
    # save matrix inverse in cache
    setinverse <- function(inverse) i <<- inverse
    
    # return matrix inverse
    getinverse <- function() i
    
    # return methods within object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    # call method to return cached inverse
    i <- x$getinverse()
    
    # if cache isn't empty, return cached data
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # otherwise, calculate matrix inverse
    data <- x$get()
    i <- solve(data, ...)
    
    # cache matrix
    x$setinverse(i)
    
    i
}
