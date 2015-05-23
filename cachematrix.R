## The two funcions below implements a matrix inverse computation cache 
## for programming assignment 2.

## makeCacheMatrix creates a wrapper for a matrix. 
## The wrapper stores the previously computed inverse matrix (if any).
## It stores a copy of the matrix when the inverse is computed.
## The copy is useful to find if the cache is still valid.

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    refMatrix <- NULL
    
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inv){
        refMatrix <<- matrix(x, dim(x))
        invMatrix <<- inv
    }
    getInverse <- function() invMatrix
    
    # We consider the matrix has changed (and cache invalidated) if: 
    # - a referral copy matrix exists (ie an inverse was previsously computed)
    # - the numbers of columns or rows are different
    # - or the absolute difference between the copy and x is not 0.
    hasChanged <- function(){
        !is.null(refMatrix) && !(ncol(x) == ncol(refMatrix) 
            && nrow(x) == nrow(refMatrix) 
            && sum(abs(refMatrix - x)) == 0)
    }
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse,
         hasChanged = hasChanged)
}


## cacheSolve returns the inverse matrix of the wrapped matrix in parameter x.
## If the inverse is computed for the first time or if the underlying matrix has changed,
## the "solve" function is actually used and the result stored in the wrapper cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    if(is.null(inv) || x$hasChanged()){
        print("compute inverse")
        inv <- solve(x$get(), ...)
        x$setInverse(inv)
    }
    
    inv
}
