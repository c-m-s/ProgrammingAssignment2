## Return the inverse of any square invertible matrix. Also cache the
## inverted matrix and return cached version if called again.
##
## Example Use: 

## build a test matrix, "myMatrix", to use for the test
## myMatrix = rbind(c(1, -1/4), c(-1/4, 1))

## instantiate a list object, "myMatrixObject", that contains the set, get,
## setinversematrix and getinversematrix methods.
## myMatrixObject <- makeCacheMatrix(myMatrix)

## send the list object, "myMatrixObject", which contains the matrix into 
## cacheSolve.
## myInversedMatrix <- cacheSolve(myMatrixObject)

## Creates a list object containing a matrix and adds methods to allow 
## the object to cache and later retrieve the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        # Adds a "set()" method to the list object.
        # "<<-" is the same as "<-" except it 
        # looks a level up to the parent environment, 
        # in this case makeCacheMatrix(). "m" is 
        # defined within makeCacheMatrix() NOT in set()
        # so it must look outside of itself.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # Adds a "get()" method to the list object.
        get <- function() {
                x
        }
        
        # Adds a "setinversematrix()" method to the list object.
        setinversematrix <- function(solve) {
                m <<- solve
        } 
        
        # Adds a "getinversematrix()" method to the list object.
        getinversematrix <- function() {
                m
        }
        
        # Return a list of the functions
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the cached inverse from the instance of the 
## object and returns it.
cacheSolve <- function(x, ...) {
        
        ## Try to get a cached version of the inverted matrix
        m <- x$getinversematrix()
        
        ## Check to see if have a cached matrix and if so, return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## So there wasn't a cached matrix to return. Set "data" to refer
        ## to the original matrix
        data <- x$get()
        
        ## Use solve() to invert the matrix and put results into "m"
        m <- solve(data, ...)
        
        ## Insert the inverted matrix into a cache
        x$setinversematrix(m)
        
        ## Return the inverted matrix
        return(m)
}
