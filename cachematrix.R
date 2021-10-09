## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #Set a variable for inverse
        inv <- NULL
        #set function
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #get function
        get <- function() x
        #set inverse of a matrix
        setInverse <- function(inverse) inv <<- inverse
        #get inverse of the matrix
        getInverse <- function() inv
        #creates list of all above
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        #Check inverse output
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #get the matrix
        mat <- x$get()
        #Solve the inverse of a matrix abd set inverse matrix to inv
        inv <- solve(mat, ...)
        x$setInverse(inv)
        #return inv
        inv
}
