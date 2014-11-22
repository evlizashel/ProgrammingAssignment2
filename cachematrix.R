## Hi! 
## Here is my task, hope you like the way I did it, though there were no really big space to be creative :)
## Please, don't be strict :)
##   NOTE: Firstly I have started to write code for 
## setting the matrix, not for using existing one. 
## when you try my function,
## set it like "m<- makeCacheMatrix(1:7,6,6)
## where 6s are number of cols and rows or "m$set(1:6,5,5)"
## I understand, that we may need input a given matrix, -
## - this way we just change in code "x,a,b" to "x = matrix()", 
## the same for "y,k,l" to "y"
## Thank you for understanding! 
## - Eva

makeCacheMatrix <- function(x,a,b) {
	  x <- matrix(x,a,b)	## input the matrix with number of rows and cols
        i <- NULL
        set <- function(y, k, l) { ## Set matrix with number of rows and cols
                x <<- matrix(y, k, l) 
                i <<- NULL
        }
        get <- function() x  ## Set "data"
        setsolve <- function(solve) i <<- solve ## set the calculated value
        getsolve <- function() i  ## calculates
        list(set = set, get = get,     ## create kind of object, where stores all we need for calculation
             setsolve = setsolve,
             getsolve = getsolve)

}


## I am happy it works!

cacheSolve <- function(x, ...) {
      i <- x$getsolve()                   ## checking for cached inversion
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()      ## if no cached invertion, loads data
        i <- solve(data)     ## calculates inversion
        x$setsolve(i)        ## set calculated value into our object
        i
}
