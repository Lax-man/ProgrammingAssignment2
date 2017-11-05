## These functions calculate and cache the inverse of a matrix so if the user tries to calculate
## the inverse of a matrix, the previously cached answer is returned instead of redoing the calculation

## This creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ## define the cache "inv"
        inv <- NULL
        
        ## store a matrix
        setMatrix <- function(y) {
                ## assign value of matrix y to the variable x
                x <<- y
                ## cache is flushed for assignment of new value
                inv <<- NULL
        }
        
        ## returns the stored matrix
        getMatrix <- function() {
                x
        }
        
        ## sets value of inv
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        
        ## returns inv
        getInverse <- function() {
                inv
        }
        
        ## returns a list. Each element is a function
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## This function calculates the inverse of the special "matrix" created by makeCacheMatrix and caches it.
## If the inverse has already been calculated then it will retrieve the inverse from cache

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        ## Returns cached inverse of matrix if it has already been calculated
        if (!is.null(inv)) {
                message("loading cached data")
                return(inv)
        }
        
        ## otherwise, calculate inverse of matrix if it hasn't already been cached
        mat <- x$getMatrix()
        inv <- solve(mat)
        
        ## Caches inverse if calculated in prior step
        x$setInverse(inv)
        inv
}