##Function to cache matrix and get inverse of matrix
##Date   - 22-Feb-2015
##Author - Mohammed Rasool
##Return - Special Vector Object

##The function makeCacheMatrix() creates a list which does the following 
##1. getMatrix() - Get the value of matrix
##2. setMatrix() - Set the value of matrix
##3. getInvMatrix - Get the inverse of matrix
##4. setInvMatrix - Set the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
    invMatrix <<- NULL
    setMatrix<-function(y){
        x <<- y
        invMatrix <<- NULL
    }
    getMatrix<-function()x
    getInvMatrix<-function()invMatrix
    setInvMatrix<-function(x){
        invMatrix <<- x
    }
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated then the cachesolve() retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    invMatrix<-x$getInvMatrix()
    if(!is.null(invMatrix))
    {
        print("Getting Cached Inv Matrix")
        return(invMatrix)
    }
    matrix<-x$getMatrix()
    invMatrix<-solve(matrix)
    x$setInvMatrix(invMatrix)
    invMatrix
}