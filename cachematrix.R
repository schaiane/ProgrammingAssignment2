## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object that can cache the inverse


makeCacheMatrix <- function(x = matrix()) {
        
        ##Initializing the inverse
        
        minv <- NULL
        
        ##Setting the matrix
        
        set <- function(matrix) {
                x <<- matrix
                minv <<- NULL
        }
        
        ##Getting the matrix
        
        get <- function() {
                x
        }
        
        ##Setting the matrix inverse
        
        setinverse <- function(inverse){
                minv <<- inverse
        }
        
        ##Getting the inverse
        
        getinverse <- function() {
               minv
        }
        
        ##Returning a list if the methods
        
        list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}

        
## The cacheSolve function calculates the inverse of matrix from 
## makeCacheMatrix. If the inverse has already been calculated, it gets 
## the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse and sets it in the cache 
## via the setinverse function.


cacheSolve <- function(x, ...) {
        
        ## Returns the inverse of the matrix 'x' 
        
        m <- x$getinverse()
        
        
        ## Returns the inverse only if it has already been set
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Getting the matrix from the resulting object
        
        data <- x$get()
        
        ## Calculating the inverse
        
        m <- solve(data)
        
        ## Setting the inverse

        x$setinverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}