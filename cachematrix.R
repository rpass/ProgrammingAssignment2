## makeCacheMatrix is a function that generates a list which contrains the functions 
## needed to get, set the matrix and get/set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve solves the invers of the matrix by checking the cacheMatrix to see 
## if the inverse has already been calculated or "set" and then retrieving the 
## inverse or calculating it depending on the result of the check 

cacheSolve <- function(x, ...){
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mtrx <- x$get()
    inv <- solve(mtrx, ...)
    x$setinverse(inv)
    inv
}
