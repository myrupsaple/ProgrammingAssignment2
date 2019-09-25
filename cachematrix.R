## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(new_inverse) inv <<- new_inverse
        getinverse <- function() inv
        list(set = set, get = get, 
                setinverse = setinverse, 
                getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        message("figuring out the inverse")
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}