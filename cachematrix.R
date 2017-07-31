## function makeCacheMatrix creates matrix construct with get set members
## function cacehSolve determines if matrix has already been inverted and thus in cache or if it needs to invert then set

#
## setup CacheMatrix construct with methods
#
makeCacheMatrix <- function(x = matrix()) {
    message ("Init matrix construct...")
    MatrixInverse = NULL
    set = function(y) {
        x <<- y
        MatrixInverse <<- NULL
    }
    get = function() x
    setMatrixInverse = function(MI) MatrixInverse <<- MI
    getMatrixInverse = function() MatrixInverse

    list(set = set, get = get, setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
}

#
## worker function to determine if matrix in in cache or not
#
cacheSolve <- function(x, ...) {
    ## Return MatrixInverse of matrix 'x'
    MatrixInverse = x$getMatrixInverse()
    if (!is.null(MatrixInverse)) {
        message("getting MatrixInverse via cache....")
        return (MatrixInverse)
    }
    data = x$get()
    MatrixInverse = solve(data)
    x$setMatrixInverse(MatrixInverse)
}


# see https://people.richland.edu/james/lecture/m116/matrices/inverses.html for this example matrix and its inverse
#2x2 matrix
mym = matrix(c(7,3,-2,5), ncol = 2, nrow = 2 )
ttt = makeCacheMatrix(mym)
print ("Matrix is:")
print (ttt$get())
print ("Initial set of MatrixInverse")
print (cacheSolve(ttt))
print ("Second access of MatrixInverse")
print (cacheSolve(ttt))
print ("Third access  of MatrixInverse")
print (cacheSolve(ttt))
print ("Fourth access of MatrixInverse")
print (cacheSolve(ttt))

#3x3 matrix
print ("Updating Matrix from 2x2 to 3x3")
mym = matrix(c(3,1,5,2,-3,-1,-5,2,4), ncol = 3, nrow = 3 )
ttt$set(mym)
print ("Matrix is:")
print (ttt$get())
print ("Initial set of MatrixInverse")
print (cacheSolve(ttt))
print ("Second access of MatrixInverse")
print (cacheSolve(ttt))
print ("Third access  of MatrixInverse")
print (cacheSolve(ttt))
print ("Fourth access of MatrixInverse")
print (cacheSolve(ttt))
