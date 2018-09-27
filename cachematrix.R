## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        sett <- function(y) {
                x <<- y
                inversa <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse)  inversa <<- inverse
        getinv <- function() inversa
        list(sett = sett, get = get, setinv = setinv, getinv = getinv)
        
        
}




cacheSolve <- function(x, ...) {
        # retorno de la  matrix inversa de  'x'
        
        inversa <- x$getinv()
        if(!is.null(inversa)) {
                message("Obteniendo el resultado en cache")
                return(inversa)
        }
        data <- x$get()
        inversa <- solve(data, ...)
        x$setinv(inversa)
        inversa
}


## Write a short comment describing this function


