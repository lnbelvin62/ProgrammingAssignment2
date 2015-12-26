## The two functions defined in this file ("makeCacheMatrix" and "cacheSolve")
## are used together to determine the inverse of a given matrix and to store
## the inverse such that the inverse has to be calculated only once.
##
## See the file "test_matrix.R" in this same directory for test code
## that demonstrates that these two functions work properly.  If you
## load the "test_matrix.R" file after loading this file ("cacheMatrix.R"),
## six sets of tests will run to test the two functions defined in
## this file.
##
## Note that the instructions for this assignment say: "assume that
## the matrix supplied is always invertible".  Therefore, there is
## no error checking on the input matrix.  Instead, the "cacheSolve"
## function will throw an error if the input matrix is singular
## (i.e., not invertible).


## The "makeCacheMatrix" function creates a special type of matrix that
## can be used with the "cacheSolve" function to determine the inverse
## of the given matrix once so that the R function "solve" does not need
## to be called multiple times.  The output of this function is a list of
## four operators that can be used by the "cacheSolve" function to get
## the inverse of the given matrix.

makeCacheMatrix <- function(internal_input_matrix = matrix()) {

    # When we are creating the cache matrix object, we do not know what
    # the inverse of the given / input matrix is, so we will set the
    # internal inverse matrix to null.
    internal_inverse_matrix <- NULL

    # The internal "set" function stores the matrix that is passed to
    # the "set" function (we call it the "set_input_matrix") as the
    # internal input matrix for use by the "get" function.
    # This internal "set" function also sets the internal inverse
    # matrix to null, because when we are first using the internal "set"
    # function, we do not know what the inverse of the "set_input_matrix" is.
    # Note the use of the "<<-" assignment operator, which causes a search
    # to be made through the parent environment for an existing definition
    # of "internal_input_matrix" and "internal_inverse_matrix".
    # Using the "<<-" assignment operator allows us to maintain the state
    # of these two variables across multiple invocations of this function
    # or multiple invocations of a function closure that uses this function.
    internal_set <- function(set_input_matrix) {
        internal_input_matrix <<- set_input_matrix
        internal_inverse_matrix <<- NULL
    }

    # The internal "get" function simply returns the input matrix -
    # the matrix that was the input to the "makeCacheMatrix" function
    # when "makeCacheMatrix" was first called for the input matrix.
    internal_get <- function() internal_input_matrix

    # The internal "set inverse" function simply calls the R "solve"
    # function to compute the inverse of the matrix passed to
    # "makeCacheMatrix" at creation time.  We store the inverse matrix
    # computed by the "solve" function so that if the "get inverse"
    # function is called after the "set inverse" function has been
    # called (for a given matrix), the "get inverse" function can
    # return the previously computed (cached) inverse matrix instead
    # of calling the R "solve" function (again).
    # Note the use of the "<<-" assignment operator, which causes a search
    # to be made through the parent environment for an existing definition
    # of "internal_inverse_matrix".  See note above about why we are using
    # the "<<-" assignment operator.
    internal_set_inverse <- function(solve) internal_inverse_matrix <<- solve

    # The internal "get inverse" function simply returns the internal
    # inverse matrix.  If the inverse for the input matrix has never been
    # computed, then "internal_inverse_matrix" will be null (because
    # the internal inverse matrix is set to null in the first statement
    # of this "makeCacheMatrix" function).  If the "set inverse" function
    # has been previously called for the input matrix, then the
    # "internal_inverse_matrix" will have already been set to the
    # inverse of the matrix passed to "makeCacheMatrix" at creation time.
    internal_get_inverse <- function() internal_inverse_matrix

    # The output of the "makeCacheMatrix" function is a list of the
    # four operations that can be performed on this special type
    # of matrix.
    list(set = internal_set, get = internal_get,
         set_inverse = internal_set_inverse,
         get_inverse = internal_get_inverse)
}


## The "cacheSolve" function takes a special type of matrix created by the
## "makeCacheMatrix" function and returns the inverse of the matrix that was
## passed to the "makeCacheMatrix" function at creation time.  The
## "cacheSolve" function uses three of the four operators provided in the
## special matrix returned by "makeCacheMatrix" (the "set" operator is not
## used anywhere but is provided for completeness).  If the inverse matrix
## has already been previously determined, this function will return the
## cached inverse as provided by the "get_inverse" operator defined in
## "makeCacheMatrix".  Otherwise, this function will call the R function
## "solve" to get the inverse and will then call the "set_inverse"
## operator to cache the inverse matrix.

cacheSolve <- function(cache_matrix, ...) {

    # Call the "get_inverse" function of the given cache matrix.
    # The given cache matrix is a special type of matrix that is
    # actually a list of four operations.  This list is generated
    # by the "makeCacheMatrix" function defined above.
    inverse_matrix <- cache_matrix$get_inverse()

    # Check whether the inverse matrix is null.  If this function
    # is being called for the first time for the given input
    # cache matrix, the inverse matrix will be null.  But if this
    # function is being called for the second or third or later time,
    # the "get_inverse" operator defined above will have cached the
    # inverse matrix via the "<<-" assignment operator.  In that case,
    # "inverse_matrix" will not be null - it will instead be the
    # actual inverse matrix, so we will return that matrix and
    # print a message to let the user know that the cached value
    # is being returned.
    if (!is.null(inverse_matrix)) {
        message("getting cached inverse")
        return(inverse_matrix)
    }

    # If execution reaches this statement, this function is being called
    # for the first time.  We need to get the actual input matrix that was
    # passed to the "makeCacheMatrix" function defined above via the
    # "get" operator.
    input_matrix <- cache_matrix$get()

    # Now we can call the R "solve" function for the input matrix.
    # We use the "..." argument in case the caller of this function
    # passed in a setting for the tolerance for detecting linear
    # dependencies in the columns of the input matrix.
    inverse_matrix <- solve(input_matrix, ...)

    # Now that we have the inverse matrix, we want to cache the
    # inverse matrix by calling the "set_inverse" operator.
    cache_matrix$set_inverse(inverse_matrix)

    # Return a matrix that is the inverse of the given "cache_matrix".
    inverse_matrix
}
