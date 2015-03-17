################################################################################
## Two functions here, namely makeCacheMatrix() and cacheSolve(), demonstrate
## how we could cache a computationally expensive object for later use
## exploiting the scoping rules of R proramming language.
##
##
## Typical usage of these two functions is here:
## > a <- matrix( c( 11, 67, 13, 14, 45, 16, 17, 18, 19 ), nrow = 3, ncol = 3 )
## > mat <- makeCacheMatrix( a )
## > mat$getMatrix()
##      [,1] [,2] [,3]
## [1,]   11   14   17
## [2,]   67   45   18
## [3,]   13   16   19
## > cacheSolve( mat )
## [1] "Calculating inverse of the matrix..."
##           [,1] [,2]      [,3]
## [1,] -18.90000 -0.2  17.10000
## [2,]  34.63333  0.4 -31.36667
## [3,] -16.23333 -0.2  14.76667
## > mat$getInverse()
##           [,1] [,2]      [,3]
## [1,] -18.90000 -0.2  17.10000
## [2,]  34.63333  0.4 -31.36667
## [3,] -16.23333 -0.2  14.76667
## > cacheSolve( mat )
## [1] "Retrieving the inverse from the cache..."
##           [,1] [,2]      [,3]
## [1,] -18.90000 -0.2  17.10000
## [2,]  34.63333  0.4 -31.36667
## [3,] -16.23333 -0.2  14.76667
## > mat$setElement( 1, 1, 190 )
## [1] 190
## > mat$getInverse()
## NULL
## > cacheSolve( mat )
## [1] "Calculating inverse of the matrix..."
##              [,1]          [,2]        [,3]
## [1,]  0.005588244  5.913486e-05 -0.00505603
## [2,] -0.010240186  3.340134e-02 -0.02248110
## [3,]  0.004799779 -2.816790e-02  0.07502242
## > mat$setMatrix(matrix(c(2,5,3,9), 2, 2))
##      [,1] [,2]
## [1,]    2    3
## [2,]    5    9
## > cacheSolve( mat )
## [1] "Calculating inverse of the matrix..."
##           [,1]       [,2]
## [1,]  3.000000 -1.0000000
## [2,] -1.666667  0.6666667
## > cacheSolve( mat )
## [1] "Retrieving the inverse from the cache..."
##           [,1]       [,2]
## [1,]  3.000000 -1.0000000
## [2,] -1.666667  0.6666667
################################################################################

################################################################################
## Purpose:
## Creates a special "matrix" object that can cache its inverse. Returns a list
## having following functions.
##      getMatrix() : returns the matrix
##      getInverse(): returns the inverse matrix
##      setMatrix() : sets the matrix
##      setElement(): sets individual elements of the matrix
##      setInverse(): sets inverse of the matrix
## Whenever the matrix is modified, its inverse is reset to NULL. Hence,
## whenever matrix is modified, user need to set the inverse as well. If the
## metrix is set an the matrix is identical to the previous matrix, it's
## inverse is kept as it is.
##
## Input object(s):
##      x: input matrix, an optional parameter
##
## Return object:
##      a list of all functions to manipulate on the matrix and inverse
##
## Warning:
##      Please note that this function will not work as expected if a
##      non-invertible matrix is given.
################################################################################
makeCacheMatrix <- function( x = matrix() ) {
    
    # Initialize inverse with NULL
    inverse_x <- NULL
    
    # Sets matrix, checks for valid matrix. Returns NULL if the input is not a
    # valid matrix. Returns the same matrix if setMatrix operation succeeds or
    # the new matrix is identical to the previous one.
    setMatrix <- function( mat ) {
        
        # Validate the input matrix
        if( !is.matrix( mat ) ) {    # VAlid matrix?
            print( "Set fail: not a matrix" )
            return( NULL )
        }
        
        ## It could have been better if we would add checking of invertible
        ## matrix before going for inverse calculation. But, let us at this
        ## point assume that user will provide a square matrix.
        
        # Skip if the new matrix is identical with the previous, if the matix
        # has not been changed no need to reset the inverse
        if( identical( x, mat ) ) {
            print( "Warning: identical with previous matrix" )
            return( x )
        }
        
        # Set the matrix
        x <<- mat
        
        inverse_x <<- NULL
        
        return( x )
    }
    
    # Sets individual element of the matrix, resets the inverse if the new value
    # of the element is not equal to the previous value.
    setElement <- function( row, col, val ) {
        if( is.na( x[row, col] ) || x[row, col] != val ) {
            x[row, col] <<- val
            
            inverse_x <<- NULL
        }
        
        # Return the value itself
        return( val )
    }
    
    # Returns the matrix to the outer world
    getMatrix <- function() {
        x
    }
    
    # Retuns the inverse of the matrix
    getInverse <- function() {
        inverse_x
    }
    
    # Sets the inverse
    setInverse <- function( mat ) {
        inverse_x <<- mat
    }
    
    # Return the list of all the functions
    list( getMatrix = getMatrix, getInverse = getInverse,
          setMatrix = setMatrix, setElement = setElement,
          setInverse = setInverse )
}


################################################################################
## Purpose:
## Returns the inverse of the special "matrix" object which is created by
## makeCacheMatrix() function. It first ask for the inverse from the cache. If
## the cache contains the inverse then returns the inverse from the cache. If
## the cache does not contain the inverse then calculates the same, set in the
## cache and retuns to the user.
##
##
## Input object(s):
##      x: special "matrix" object returned by makeCacheMatrix()
##
## Return object:
##      Inverse of input matrix x
################################################################################
cacheSolve <- function( x, ... ) {
    
    # Get the inverse matrix
    inverse <- x$getInverse()
    
    # Inverse is not NULL: means we are getting inverse from cache
    if( !is.null( inverse ) ) {
        print( "Retrieving the inverse from the cache..." )
        return( inverse )
    }
    
    # So, inverse is not there in cache, calculate, set in cache and return
    print( "Calculating inverse of the matrix..." )
    inverse <- solve( x$getMatrix() )
    x$setInverse( inverse )
    
    return( inverse )
}
