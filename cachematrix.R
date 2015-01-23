
## Creates a matrix that can catche its inverse

B<-matrix(4:7,nrow=2,ncol=2)


makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        	set <- function(y) {							##Sets the value of the matrix
                x <<- y
                m <<- NULL
        	}
        	get <- function() x							##Gets the value of the matrix
        	setmatrix <- function(solve) m <<- matrix		##Sets the value of the inverse
        	getmatrix <- function() m						##Gets the value of the inverse
        	list(set = set, get = get,						##Lists the functions with names
         	    	setmatrix = setmatrix,
             		getmatrix = getmatrix)
}


## Computes the inverse of the matrix returned by makeCatcheMatrix and if it has already been calculated then retrieves it from the cache

cacheSolve <- function(x, ...) {
m <- x$getmatrix()									##Calls the function getmatrix from makeCacheMatrix
        if(!is.null(m)) {								##If m is not null, message appears and matrix m is returned	
                message("getting cached data")
                return(m)
        }
        data <- x$get()								##Else it uses the get function to get the value of the matrix
        m <- solve(data, ...)							##Uses solve to calculate the inverse of the matrix
        x$setmatrix(m)									##Sets value of matrix m	
        m
}

        ## Return a matrix that is the inverse of 'x'
