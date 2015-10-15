## The following functions work together to be able to cache the inverse of a matrix. Inverting a large
##matrix is computationally intensive and it makes sense to be able to cache the inverse

## The function makeCacheMatrix creates a special matrix object; this object is really as list that contains:
# - a function to get the matrix
# - a function to set the matrix
# - a function to get the inverse of the matrix
# - a function to set the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                inv <<- NULL
                x <<- y
        }
        get <- function() x
        setInverse <- function(y){ inv <<- y }
        getInverse <- function() inv
        return( list(set = set, get = get, setInverse = setInverse, getInverse = getInverse ))
}


## The function CacheSolve uses the makeCacheMatrix function to return the inverse of a matrix
# it checks whether the inverse was already cached and if so, returns the cached version
# if not, it calculates the inverse and adds it to the cache (using the "set inverse" function on the special matrix object)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){ #matrix inverse found   
                message("getting cached data")
                return(inv)
        }
        #calculate inverse and add to cache; assumed that x  is invertible
        obj <- x$get()
        inv <- solve(obj)
        x$setInverse(inv)
        inv
}


