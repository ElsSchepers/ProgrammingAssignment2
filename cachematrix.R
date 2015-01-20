## The purpose of this code is to cache the inverse of a matrix
## so that when it is needed again, it can be looked up in the cache rather than recomputed.

## A matrix is invertible IF AND ONLY IF its determinant DOES NOT EQUAL zero.

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse
## The cached inverse will be stored in the variable m

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinvmatrix <- function(solve) m <<- solve
    getinvmatrix <- function() m
    list(set=set, get=get,
         setinvmatrix=setinvmatrix,
         getinvmatrix=getinvmatrix)
    
}

## The function cacheSolve computes the inverse of the special "matrix" returned by 
## function makeCacheMatrix . 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
   
    m <- x$getinvmatrix()
    if(!is.null(m)){
        message("looking up cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setinvmatrix(m)
    m
    
}

