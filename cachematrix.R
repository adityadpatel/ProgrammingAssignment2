## I have here built two functions viz "makeCacheMatrix" and "cacheSolve". MakeCacheMAtrix takes an Invertible Matrix as input and returns  List as an output. The list contains 4 separate functions
## set: -  If the matrix has changed, the set function sets the new value in the list, while erasing the previously cached Inverse of the matris
## get: - is a function that returns the matrix
## setinverse : - takes the inverse of the matrix as an argument and caches it
## getinverse: - returns the cached inverse of the matrix. If the inverse is not cached, it returns null


makeCacheMatrix <- function(x = matrix()) 
{

 inv<- NULL
        
        
        set <- function(y)
        {
                x <<- y
                inv <<- NULL
        }
        get <- function() 
        {
                x
        }
        setinv <- function(inverse) 
        {
                inv <<- inverse
        }
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv
        )
}


## the function cacheSolve takes the output of makeCacheMatrix as Input. The statement "cacheSolve(makeCacheMatrix(m))" is valid.
## this function first checks whether the inverse is already cached. If yes, the inverse of the matrix which is already cached is returned ... thus saving computation expense.
## If the inverse is not already cached, this function solves to compute the inverse first and stores the same in the cache using the setinv() function. It also returns the recently computed inverse of the matrix.


cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
		 inv <- x$getinv()
        if(!is.null(inv)) 
        {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setinv(inv)
        inv
}
