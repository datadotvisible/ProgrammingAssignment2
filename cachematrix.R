## These two functions make use of the R languages Lexical Scoping to help store matrix calculations within
## the parent environments of the functions.  This is accomplished by use of the <<- method for assigning values.

## makeCacheMatrix takes as its argument x a matrix, if none is supplied an empty one will be created.
## It then creates a set of functions which access data values in the parent scope.
## set, get, setmatrix, and getmatrix are accessed via function completions within R.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setmatrix <- function(solve) m <<- solve
   getmatrix <- function() m
   list(set=set, get=get,
        setmatrix=setmatrix,
        getmatrix=getmatrix)

}


## cacheSolve takes a matrix (preferrably one created with makeCacheMatrix)
## and if it can find a copy of the matrix in the parent environment uses that matrix to solve.
## This also uses very tightly coupled knowledge of the functions available within the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
	m <- x$getmatrix()
	if (!is.null(m)) {
		return(m)
	}
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setmatrix(m)
	return (m)
}
