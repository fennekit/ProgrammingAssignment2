## These functions implement functionaly to store calculate an inverse matrix and cache the
## inverse matrix to improve performance

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(inputMatrix = matrix()) {
    cachedInverse <- NULL
    set <- function(newInputMatrix) {
        inputMatrix <<- newInputMatrix
        cachedInverse <<- NULL
    }
    get <- function() {
        inputMatrix
    }
    setInverseMatrix <- function(newInverseMatrix) {
        cachedInverse <<- newInverseMatrix
    }
    getInverseMatrix <- function() {
        cachedInverse
    }
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverseMatrix()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverseMatrix(inv)
    inv
}
