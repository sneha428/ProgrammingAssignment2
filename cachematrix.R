##Matrix inversion is usually a costly computation and there may be some benefit to 
##caching the inverse of a matrix rather than compute it repeatedly.
##The following pair of functions will cache the inverse of a matrix.

## The function, makeCacheMatrix, creates a special "matrix", which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## cacheSolve function caluclates the inverse of the special "matrix" created in makeCacheMatrix.
## It first checks to see if the inverse is already calculated. If so, then it gets the the inverse of the matrix
## from the cache and skips the caluclation. If not, then it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

