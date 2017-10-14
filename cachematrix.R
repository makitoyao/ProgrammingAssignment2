## Put comments here that give an overall description of what your
## functions do



# This function creates a special "matrix" object that can cache its inverse
# first creat a variable call x as x<-makeCacheMatrix()
# set: set a matrix to x
# get: get matrix from x
# setInverse: set inverse matrix to list x
# getInverse: get inverse matrix from x, if exsists

makeCacheMatrix <- function(x = matrix()) {
        inverse_x <- NULL
        
        # set matrix to variable x
        set <- function(y) {
                if (class(y) == 'matrix'){
                        x <<- y
                        inverse_x <<- NULL
                }
                else{
                        print('Input is not a matrix')
                }
        }
        
        # get matrix from variable x
        get <- function() {
                x
        }
        
        # set inverse matrix 
        setInverse <- function(inverseToSet) {
                inverse_x <<- inverseToSet
        }
        
        # get inverse matrix
        getInverse <- function() {
                inverse_x
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}




# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the 
# inverse from the cache.
cacheSolve <- function(x, ...) {
        inverse_x <- x$getInverse()
        if(!is.null(inverse_x)) {
                message("getting cached data")
                return(inverse_x)
        }
        data <- x$get()
        inverse_x <- solve(data, ...)
        x$setInverse(inverse_x)
        inverse_x
        ## Return a matrix that is the inverse of 'x'
}
