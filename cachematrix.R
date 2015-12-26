## Put comments here that give an overall description of what your
## functions do
##
## Note that the instructions for this assignment say: "assume that
## the matrix supplied is always invertible".  Therefore, there is
## no error checking on the input matrix.  Instead, the "cacheSolve"
## function will throw an error if the input matrix is singular
## (not invertible).

## Write a short comment describing this function

makeCacheMatrix <- function(internal_input_matrix = matrix()) {

    ## When we are creating the cache matrix object, we do not know what
    ## the inverse of the given / input matrix is, so we will set the
    ## internal inverse matrix to NULL.
    internal_inverse_matrix <- NULL

    ## The internal "set" function stores the matrix that is passed to
    ## the "set" function (we call it the "set_input_matrix") as the
    ## internal input matrix for use by the "get" function.
    ## This internal "set" function also sets the internal inverse
    ## matrix to NULL, because when we are first using the internal "set"
    ## function, we do not know what the inverse of the "set_input_matrix" is.
    internal_set <- function(set_input_matrix) {
        internal_input_matrix <<- set_input_matrix
        internal_inverse_matrix <<- NULL
    }

    ## The internal "get" function simply returns the input matrix -
    ## the matrix that was the input to the "makeCacheMatrix" function
    ## when "makeCacheMatrix" was first called for the input matrix.
    internal_get <- function() internal_input_matrix

    ## The internal "set inverse" function simply calls the R "solve"
    ## function to compute the inverse of the matrix passed to
    ## "makeCacheMatrix" at creation time.  We store the inverse matrix
    ## computed by the "solve" function so that if the "get inverse"
    ## function is called after the "set inverse" function has been
    ## called (for a given matrix), the "get inverse" function can
    ## return the previously computed (cached) inverse matrix instead
    ## of calling the R "solve" function (again).
    internal_set_inverse <- function(solve) internal_inverse_matrix <<- solve

    ## The internal "get inverse" function simply returns the internal
    ## inverse matrix.  If the inverse for the input matrix has never been
    ## computed, then "internal_inverse_matrix" will be NULL (because
    ## the internal inverse matrix is set to NULL in the first statement
    ## of this "makeCacheMatrix" function).  If the "set inverse" function
    ## has been previously called for the input matrix, then the
    ## "internal_inverse_matrix" will have already been set to the
    ## inverse of the matrix passed to "makeCacheMatrix" at creation time.
    internal_get_inverse <- function() internal_inverse_matrix

    ## The output of the "makeCacheMatrix" function is a list of the
    ## four operations that can be performed on this special type
    ## of matrix.
    list(set = internal_set, get = internal_get,
         set_inverse = internal_set_inverse,
         get_inverse = internal_get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- x$get_inverse()
    if (!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$set_inverse(m)

    ## Return a matrix that is the inverse of 'x'
    m
}
