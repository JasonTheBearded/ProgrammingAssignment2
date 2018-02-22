##Set of functions that will take a matrix in and output the inverse matrix of that matrix. 

##Create the object that can house the inverse of a matrix
makeCacheMatrix <- function(m = matrix()){
    ##Create an empty inverse vector
    inv <- NULL
    ##Create function to set vector variables for use outside of function
    set <- function(n){
        m <<- n
        inv <<- NULL
    }
    get <- function() m
    setInv <- function(solveMatrix) inv <<- solveMatrix
    getInv <- function() inv
}

##Compute the inverse of the matrix returned by makeCacheMatrix
cacheSolve <- function(x, ..){
    inv <- x$getInv()
    ##Check to see if inverse matrix has already been created
    if(!is.null(inv)){
        return(inv)
    }
    ##Return the inverse matrix
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    return(inv)
}
