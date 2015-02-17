## The functions here are designed to cache a matrix inverse in order to save computation time.
## One function creates an object to store data and the other reads it or computes the inverse.

## This function creates a special list of functions to manipulate a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL           # Initialize the inverse to NULL.
        set <- function(y) {
                x <<- y       # Overwrite x and set the new value as y.
                inv <<- NULL  # Since the matrix is overwritten we reset inv.
        }
        get <- function() { x }                              # Retrieve the matrix x.
        setinverse <- function(inverse) { inv <<- inverse }  # Set the inverse to specified value.
        getinverse <- function() { inv }                     # Retrieve the inverse.
        
        # Put all these functions into a list labelled by their names.
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function retrieves the inverse if it's already computed. If not, the inverse is computed.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()        #Look up the inverse (NULL if it hasn't been computed yet).
        if(!is.null(inv)) {          #If the inverse has been computed print the following:
                message("Getting cached data.")
                return(inv)          #This stops the function from evaluating the next lines.
        }
        matrix <- x$get()            #Retrieve the matrix to be inverted.
        inv <- solve(matrix, ...)    #Compute the inverse.
        x$setinverse(inv)            #Store the inverse for future use.
        inv                          #Return the inverse as the value of the function.
}
