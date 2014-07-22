##This code makes it possible to store a matrix and use functions
##associated with the object to get info about the matrix and its inverse.
##There is also a function that computes the inverse of the matrix
##and stores the value of the inverse.

## Calling makeCacheMatrix sets the matrix.  The returned object
## has the capability to set/get the matrix and set/get the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {

        matrix_inverse <- NULL

        #Sets (or resets) matrix.  The matrix inverse it reset to NULL if the matrix is reset.
        set_matrix <- function(y) {
               x <<- y 
               matrix_inverse <<- NULL  #Set inverse to null if matrix is set/reset
        }

        #Retrieves matrix
        get_matrix <- function()  {
               return(x)   }

        #Sets the inverse of the matrix
        set_matrix_inverse <- function(inverse) {
               message("We are setting the inverse.")
               matrix_inverse <<- inverse
        }

        #Retrieves matrix inverse
        get_matrix_inverse <- function() {
               return(matrix_inverse)  }
 
        #This is a list of functions that can be called by the object.
        list(set_matrix=set_matrix, get_matrix=get_matrix, set_matrix_inverse=set_matrix_inverse, 
              get_matrix_inverse=get_matrix_inverse)
              
}


## This function checks to see if the matrix inverse has been computed.  If so,
## it returns that value.  If not, it computes and stores the matrix's inverse
## for future use.

cacheSolve <- function(x, ...) {

       if(!is.null(x$get_matrix_inverse())) {
         message("Good news! We already have the inverse stored!")
         return(x$get_matrix_inverse()) 
       }

       matrix_inverse = solve(x$get_matrix())
       x$set_matrix_inverse(matrix_inverse)
       return(matrix_inverse)
       


}
