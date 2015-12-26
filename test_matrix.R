## Test code for the "cacheSolve" and "makeCacheMatrix" functions that
## are defined in "cachematrix.R".  If all of the tests in this file pass,
## then we can have some confidence that the "cacheSolve" and
## "makeCacheMatrix" functions are correct.

## We first need to define a function that will test for matrix equality.
## Note that in R, except for the simplest matrices (2 x 2 and 3 x 3,
## generally), the "solve" function will return floating point values that
## are often slightly different than the exact correct answer, especially
## when the correct answer is a matrix with all integer values.  So we will
## test each element of the matrix for equality within a given small value
## (epsilon).
are_matrices_equal <- function(matrix_a, matrix_b, epsilon=1e-8) {

    # First check whether the dimensions of the two given matrices
    # are the same.
    if (!(dim(matrix_a)[1] == dim(matrix_b)[1]) &&
         (dim(matrix_a)[2] == dim(matrix_b)[2])) {
        message("are_matrices_equal - input dimensions are different")
        return(FALSE)
    }

    # Subtract one matrix from the other.  If the two matrices are
    # equal or nearly equal, their difference should contain elements
    # that are all zero or nearly zero.
    matrix_diff = matrix_a - matrix_b

    # Now verify that all of the elements are zero or nearly zero.
    for (row in seq(nrow(matrix_diff))) {
        for (col in seq(ncol(matrix_diff))) {
            if (matrix_diff[row, col] > epsilon) {
                return(FALSE)
            }
        }
    }

    return(TRUE)
}


## Define six matrices for testing "cacheSolve" and "makeCacheMatrix".
## Note that these matrices must all be square matrices because only
## square matrices are invertible.
A = matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
B = matrix(c(2, 3, 4, 5), nrow=2, ncol=2)
C = matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), nrow=3, ncol=3)
D = matrix(c(2, 1, 0, 0, 3, 0, 2, 2,
             1, 3, -3, 3, 5, 1, 2, 1), nrow=4, ncol=4)
E = matrix(c(18, 4, 4, 5, 19, 23, 17, 25, 2, 18,
             6, 11, 23, 16, 24, 22, 0, 8, 30, 15,
             37, 1, 26, 29, 0, 23, 7, 31, 23, 39,
             37, 22, 0, 21, 26, 21, 3, 20, 16, 11,
             32, 0, 15, 34, 7, 30, 3, 27, 32, 19,
             30, 17, 2, 18, 3, 38, 18, 28, 38, 37,
             16, 21, 3, 8, 29, 13, 19, 23, 9, 2,
             6, 3, 4, 19, 29, 9, 15, 40, 5, 38,
             14, 22, 11, 34, 14, 2, 38, 7, 35, 14,
             20, 37, 30, 1, 11, 4, 39, 28, 35, 37), nrow=10, ncol=10)
F = matrix(c(1, 0, -3, 2, 0, 0, 0, 0, -3, 0, 9, -6, 2, 0, -6, 4,
             0, 0, 3, -2, 0, 0, 0, 0, 0, 0, -9, 6, 0, 0, 6, -4,
             0, 0, 0, 0, 0, 0, 0, 0, 3, 0, -9, 6, -2, 0, 6, -4,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, -6, 0, 0, -6, 4,
             0, 1, -2, 1, 0, 0, 0, 0, 0, -3, 6, -3, 0, 2, -4, 2,
             0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 3, -3, 0, 0, -2, 2,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 3, -6, 3, 0, -2, 4, -2,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -3, 3, 0, 0, 2, -2,
             0, 0, 0, 0, 1, 0, -3, 2, -2, 0, 6, -4, 1, 0, -3, 2,
             0, 0, 0, 0, 0, 0, 3, -2, 0, 0, -6, 4, 0, 0, 3, -2,
             0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 3, -2, 1, 0, -3, 2,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -3, 2, 0, 0, 3, -2,
             0, 0, 0, 0, 0, 1, -2, 1, 0, -2, 4, -2, 0, 1, -2, 1,
             0, 0, 0, 0, 0, 0, -1, 1, 0, 0, 2, -2, 0, 0, -1, 1,
             0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 2, -1, 0, 1, -2, 1,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, -1, 1),
             nrow=16, ncol=16)

## The "I" below stands for "inverse".  So "IA" is the inverse of matrix A.
## (The inverse of E has non-integer values so I skipped defining it.)
IA = matrix(c(-2, 1, 1.5, -0.5), nrow=2, ncol=2)
IB = matrix(c(-2.5, 1.5, 2, -1), nrow=2, ncol=2)
IC = matrix(c(-24, 20, -5, 18, -15, 4, 5, -4, 1), nrow=3, ncol=3)
ID = matrix(c(18, 9, -2, -12, -35, -18, 4, 24,
              -28, -14, 3, 19, 1, 1, 0, -1), nrow=4, ncol=4)
IF = matrix(c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 1, 0, 1, 0, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 1, 0, 1, 0, 3, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
              0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
              0, 0, 0, 1, 0, 0, 0, 2, 0, 1, 0, 1, 0, 2, 0, 2,
              0, 0, 0, 1, 0, 0, 0, 3, 0, 1, 0, 1, 0, 3, 0, 3,
              0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 0,
              0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 2, 0, 0, 2, 2,
              0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 4,
              0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 2, 0, 0, 0, 6,
              0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0,
              0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 3, 0, 0, 3, 3,
              0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 6,
              0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 9),
              nrow=16, ncol=16)

## The "MCM" below stands for "makeCacheMatrix" - the first of the two
## functions that we had to write for this assignment.  So "A_MCM" is the
## output from "makeCacheMatrix" when "makeCacheMatrix" is given the
## matrix "A" as its input.  Note that A_MCM is not a true matrix - instead,
## it is a list of four operations (set, get, set inverse, and get inverse)
## that can be performed on the matrix that is passed in as the input to the
## "makeCacheMatrix" function.
A_MCM = makeCacheMatrix(A)
B_MCM = makeCacheMatrix(B)
C_MCM = makeCacheMatrix(C)
D_MCM = makeCacheMatrix(D)
E_MCM = makeCacheMatrix(E)
F_MCM = makeCacheMatrix(F)

## The "CS" below stands for "cacheSolve" - the second of the two functions
## that we had to write for this assignment.  So "IA_CS" is the output from
## "cacheSolve" when "cacheSolve" is given the special matrix (list of
## four operations) that was created by the "makeCacheMatrix" call.
## On my computer, solving for the inverse of the 16 x 16 matrix is
## essentially instantaneous, so we cannot compare execution times
## to confirm that the cached value of the inverse is being used
## (instead of calling the R function "solve" each time).  Instead,
## we will rely on the message that is displayed that indicates that
## the cached value is being used.
IA_CS_1 = cacheSolve(A_MCM)
IA_CS_2 = cacheSolve(A_MCM)
IA_CS_3 = cacheSolve(A_MCM)

IB_CS_1 = cacheSolve(B_MCM)
IB_CS_2 = cacheSolve(B_MCM)
IB_CS_3 = cacheSolve(B_MCM)

IC_CS_1 = cacheSolve(C_MCM)
IC_CS_2 = cacheSolve(C_MCM)
IC_CS_3 = cacheSolve(C_MCM)

ID_CS_1 = cacheSolve(D_MCM)
ID_CS_2 = cacheSolve(D_MCM)
ID_CS_3 = cacheSolve(D_MCM)

IE_CS_1 = cacheSolve(E_MCM)
IE_CS_2 = cacheSolve(E_MCM)
IE_CS_3 = cacheSolve(E_MCM)

IF_CS_1 = cacheSolve(F_MCM)
IF_CS_2 = cacheSolve(F_MCM)
IF_CS_3 = cacheSolve(F_MCM)

## We need to verify that the results from successive calls to the
## "cacheSolve" function are the same.
stopifnot(IA_CS_1 == IA_CS_2)
stopifnot(IA_CS_1 == IA_CS_3)

stopifnot(IB_CS_1 == IB_CS_2)
stopifnot(IB_CS_1 == IB_CS_3)

stopifnot(IC_CS_1 == IC_CS_2)
stopifnot(IC_CS_1 == IC_CS_3)

stopifnot(ID_CS_1 == ID_CS_2)
stopifnot(ID_CS_1 == ID_CS_3)

stopifnot(IE_CS_1 == IE_CS_2)
stopifnot(IE_CS_1 == IE_CS_3)

stopifnot(IF_CS_1 == IF_CS_2)
stopifnot(IF_CS_1 == IF_CS_3)

## We also need to verify that cacheSolve returned the inverse matrix of
## the original matrix that was passed to "makeCacheMatrix".  We will do
## this by multiplying (matrix multiplication) the original matrix and
## the inverse in both possible orders and checking whether the result
## is the identity matrix.  Use the "diag" function to create the
## identity matrices.  Also check the inverse matrix against the known
## inverse (except for "E").
IDENT_A = diag(2)
ident_A_1 <- A %*% IA_CS_1
ident_A_2 <- IA_CS_1 %*% A
stopifnot(are_matrices_equal(ident_A_1, ident_A_2))
stopifnot(are_matrices_equal(IDENT_A, ident_A_1))
stopifnot(are_matrices_equal(IA, IA_CS_1))

IDENT_B = diag(2)
ident_B_1 <- B %*% IB_CS_1
ident_B_2 <- IB_CS_1 %*% B
stopifnot(are_matrices_equal(ident_B_1, ident_B_2))
stopifnot(are_matrices_equal(IDENT_B, ident_B_1))
stopifnot(are_matrices_equal(IB, IB_CS_1))

IDENT_C = diag(3)
ident_C_1 <- C %*% IC_CS_1
ident_C_2 <- IC_CS_1 %*% C
stopifnot(are_matrices_equal(ident_C_1, ident_C_2))
stopifnot(are_matrices_equal(IDENT_C, ident_C_1))
stopifnot(are_matrices_equal(IC, IC_CS_1))

IDENT_D = diag(4)
ident_D_1 <- D %*% ID_CS_1
ident_D_2 <- ID_CS_1 %*% D
stopifnot(are_matrices_equal(ident_D_1, ident_D_2))
stopifnot(are_matrices_equal(IDENT_D, ident_D_1))
stopifnot(are_matrices_equal(ID, ID_CS_1))

# (Note that we did not define IE so we are skipping one verification.)
IDENT_E = diag(10)
ident_E_1 <- E %*% IE_CS_1
ident_E_2 <- IE_CS_1 %*% E
stopifnot(are_matrices_equal(ident_E_1, ident_E_2))
stopifnot(are_matrices_equal(IDENT_E, ident_E_1))

IDENT_F = diag(16)
ident_F_1 <- F %*% IF_CS_1
ident_F_2 <- IF_CS_1 %*% F
stopifnot(are_matrices_equal(ident_F_1, ident_F_2))
stopifnot(are_matrices_equal(IDENT_F, ident_F_1))
stopifnot(are_matrices_equal(IF, IF_CS_1))
