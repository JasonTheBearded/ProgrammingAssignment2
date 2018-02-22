##Create the matrix object that can house the inverse
makeCacheMatrix <- function(m = matrix()){
    ##Create an empty inverse vector
    inv <- NULL
    ##Create function to set vector variables for use outside of function
    set <- function(n){
        m <<- n
        inv <<- NULL
    }
    get <- function() m
    setInverse <- function(solveMatrix) inv <<- solveMatrix
    getInverse <- function() inv
}

##Compute the inverse of the matrix returned by makeCacheMatrix
cacheSolve <- function(x, ..){
    inv <- x$getInverse()
    ##Check to see if inverse matrix has already been created
    if(!is.null(inv)){
        return(inv)
    }
    ##Return the inverse matrix
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    return(inv)
}
