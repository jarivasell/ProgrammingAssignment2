## Submission for programming assignment 2
## Added more comments to the code to make all parts of the function more clear

## Usage: 
## MatrixName <- makeCacheMatrix(inputMatrix)
##
## Example:
## MatrixName <- makeCacheMatrix(matrix(c(2,8,3,3,9,2,1,5,7),3,3))

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL    # m will be set to NULL, it will contain inverse of matrix in future
    
    set <- function(y) {   # set will save the original data in previous environment to x and NULL to m
        x <<- y 
        m <<- NULL
    }
    
    #Functions below will be used by cacheSolved function defined after this
    get <- function() { x }    # get will return the data of the original input matrix
    
    setInvertMatrix <- function(inverted) { m <<- inverted }    # setInvertMatrix will store inverse matrix to m in previous environment
    
    getInvertMatrix <- function() { m }    # getInvertMatrix will return the inverted matrix m
    
    list(set = set,     # This list will be returned to the object that called this function
         get = get,
         setInvertMatrix = setInvertMatrix,
         getInvertMatrix = getInvertMatrix)
}




## cacheSolve can be only used on variables that has been created makeCacheMatrix!
## cacheSolve will call the functions that are defined above
## cacheSolve will return inverted matrix from cache if it has been stored there

cacheSolve <- function(x, ...) {    
    
    m <- x$getInvertMatrix()       ## m will get the data of the inverted matrix stored in x$getInvertMatrix
    
    if(!is.null(m)) {      ## check if m was empty and if not, adds text getting cached data and returns m
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()        ## original input matrix is stored to data
    
    m <- solve(data, ...)  ## inverted matrix is stored to m
    
    x$setInvertMatrix(m)   ## calls seInvertMatrix to set m also to the previous environment
    
    m                      ## Returns the inverted matrix m at then end of this function
    
}
