## Calculating the inverse of a matrix can be costly. To avoid unnecessary
## computations, these functions store the inverse of a matrix in a 
## cache. This prevents the inverse of a matrix from being unnecessarily 
## recalculated.

## Creates a list of functions that stores a matrix and its inverse, if 
## previously calculated. Other functions allow the user to edit the contents of
## the matrix and its inverse

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


## Attempts to load the cached data from the list created in makeMatrix.
## If a cached inverse matrix exists, it is immediately returned
## Else, the inverse is calculated and returned

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