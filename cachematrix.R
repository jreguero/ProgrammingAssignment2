## makeCacheMatriz creates an special matrix objects that allows 
## us to store a matrix and its inverse. It has submethods to 
## update and recover both the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # When we create the object we set the inverse to null
    inv <- NULL
    
    # Definition of the set fuction: store the value of the matrix
    set <- function(y) {
        # store the value
        x <<- y
        # as we change the value, clean the inverse
        inv <<- NULL
    }
    
    #get function return the mattrix
    get <- function() x
    
    #setinverse function store the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    #getinverse function return the inverse
    getinverse <- function() inv
    
    #define a list with the object functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of a matrix object created by makeCacheMatrix,
## storing its value in the same matrix object. In case the inverse of the matrix
## was computed before, it return the cached value

cacheSolve <- function(x, ...) {
    #Get the cached inverse
    inv <- x$getinverse()
    
    #if it is not null, the inverse was previously computed
    if(!is.null(inv)) {
        # return the cached inverse with a warning message
        message("getting cached data")
        return(inv)
    }
    #if the cached inverse is null, we need to compute it
    
    #recover the matrix
    data <- x$get()
    
    #compute the inverse
    inv <- solve(data, ...)
    
    #store the inverse back in the object
    x$setinverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
