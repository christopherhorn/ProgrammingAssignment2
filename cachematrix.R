## Put comments here that give an overall description of what your
## functions do

##This function creates a special "matrix" object that can cache its inverse. It does this by use of the '<<-' operator to assign
##Variable values to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        ##This function creates a special "matrix" object that can cache its inverse.

        # im is a a variable that will hold the cached inverted matrix
        im <- NULL  # im initialized to NULL

        #The 'set' block holds the function that will execute commands when invoked externally
        set <- function(y) {
                x <<- y         #The '<<-' operator pushes the y value to the x variable in the parent environment for caching
                im <<- NULL     #The im (inverted matrix) variable is NULL'ed in the parent environment
        }

        #The 'get' in-line function returns the value of x when invoked externally
        get <- function() return(x)     #'get' is a function that takes no input arguments and returns 'x'

        setInvertedMatrix <- function(invertedMatrix) im <<- invertedMatrix #This in-line function takes a matrix passed in and stores it as a variable in the parent environment

        getInvertedMatrix <- function() im #This in-line function looks for the inverted matrix locally, and failing to find it, seeks it in the parent environment

        #This list exposes the functions above to be called externally
        list(set = set, get = get,
             setInvertedMatrix = setInvertedMatrix,
             getInvertedMatrix = getInvertedMatrix)
}


## Returns a matrix that is the inverse of 'x', where x is a square matrix passed in. It does this by attempting to retrieve a cached inverted matrix, and
## passing that as output, or creating an iverted matrix where one is not already in the cache.

cacheSolve <- function(x, ...) {

        # 'im', a variable that will hold the inverted matrix, invokes the getInvertedMatrix method of makeCacheMatrix
        im <- x$getInvertedMatrix()

        # If, when searching for the inverted matrix, we find one, we return it rather than recreating it
        if(!is.null(im)) {  # Test for the invrted matrix existence
                message("getting cached data")  # We found it, so print out a message indicating this status
                return(im)                      # Return the inverted matrix
        }
        data <- x$get()                         # If we found no cached inverted matrix, invoke the get method to retrieve the source matrix
        im <- solve(data, ...)                  # Invert the source matrix and store its inverse in the 'im' variable
        x$setInvertedMatrix(im)                 # Invoke the setInvertedMatrix method of the makeCacheMatrix function to store the inverted matrix in the parent environment
        im                                      # Return the inverted matrix as an output of the function
}
