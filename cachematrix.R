##Create a matrix object that can cache its inverse; function makeCacheMatrix
##returns a list of four functions (set matrix, get matrix, inverse the matrix)
##and get the inversed matrix)


makeCacheMatrix <- function(x = numeric(),r=numeric(),c=numeric()) {
        inv <- NULL
        set <- function(y,a,b) {  #create matrix
                x<<-matrix(y,a,b)
                inv <<- NULL
        }
        get <- function() x #print matrix
        setinverse <- function(inv) inv <<- solve #inverse matrix
        getinverse <- function() inv #print inversed matrix
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#Computes the inverse of the special "matrix" returned by makeCacheMatrix.
#If the inverse has already been calculated (and the matrix has not changed),
#then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()  #assigns the inversed matrix to variable
        if(!is.null(inv)) { #if the inv variable is not empty prints the cached inversed matrix
                
                message("getting cached data")
                inv
        }
        data <- x$get()  #if the inv variable is empty, gets the original matrix
        inv<-solve(data, ...) #inverses the original matrix
        x$setinverse(inv) #assigns inversed matrix to inv variable
        inv #prints inversed matrix
}
