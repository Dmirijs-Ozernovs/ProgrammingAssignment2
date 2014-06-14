#Function creates a special "matrix" object that can cache its inverse. 
#Function contains four other functions - "set" for setting matrix, "get" for retrieving the matrix,
#"setinv" for calculating the inverse and "getinv" for retrieving cached inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x

    setinv <- function(solve) m <<- solve
    
    getinv <- function() m
        
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

#"cacheSolve" function calculates the inverse of the special "matrix" created with the "makeCacheMatrix" function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the "setinv" function.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}


