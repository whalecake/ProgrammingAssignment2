## The two functions combined cache the inverse of the matrix objects, 'makeCacheMatrix'
## and then uses the 'cacheSolve' function to retrieve any cached inverses

## returns a makeCacheMatrix object with x, i, and four objects specific to the matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        # set value of matrix, 'x' in the parent environment, and clears previously cached 'inverse'
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Requires an input object of type makeCacheMatrix
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        # if i is NOT NULL (previously cached), return cached data
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        
        # if i has NOT been previously cached, setinverse() for the object, and then return the inverse
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
