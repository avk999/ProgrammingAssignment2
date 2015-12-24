## Programming assignment 2 for "R Programming" course
##
## Matrix inversion is costly, so there may be benefit to cache the solution
## once computed. We use two functions - makeCacheMatrix(), that creates a
## special "matrix" object, that can cache its inverse, and cacheSolve(), that
## always returns the inverse of the matrix, using cached value if possible.
##
## intended use:
##
## mymatrix<-makeCacheMatrix(some_matrix)
## mymatrix$get() - returns the matrix equal to some_matrix.
## cacheSolve(mymatrix) - returns the inversion, cached if possible.
## mymatrix$set(new_matrix) - changes the matrix, invalidating the cached
## inverse.
##
## !!! no checks are made if the matrix is invertible. Please
##
## makeCacheMatrix - creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y){
                x <<- y  # note <<-, we have to set x in the parent env
                cache <<- NULL #invalidate cache if we change the matrix

        }

        get <- function () {x}
        setcache <- function(i) {cache <<- i}
        getcache <- function() {cache}
        list(set = set, get = get, setcache = setcache, getcache = getcache)
}




## return inverse of the matrix stored in the special object
## created with makeCacheMatrix(). If the solution is already cached return
## cached value, else compute the inverse, update the cache and return it.
##
## TODO: add "verbose=FALSE" parameter to suppress printing of the "Using
## cached data" line
##
## WARNING: no checks are made if the matrix is singular. Refer to solve()
## documentation.

cacheSolve <- function(x, ...) {
        if (class(x)=="matrix") { # try to do the right thing if we
                                  # got not the special object but a
                                  # normal matrix
                warning("Got not the special object but a matrix")
                return(solve(x,...))

        }

        if (is.null(x$getcache())){  # no cached data
                x$setcache(solve(x$get(),...))
        }
        else {print ("Using cached data")}
        x$getcache()

}


## test case:
# > m<-matrix(data=c(2,2,3,2),nrow=2,ncol=2)
# > my_object<-makeCacheMatrix(m)
# > my_object$get()
# [,1] [,2]
# [1,]    2    3
# [2,]    2    2
# > cacheSolve(my_object)
# [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
# > cacheSolve(my_object)
# [1] "Using cached data"
# [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
# Warning message:
#         In cacheSolve(m) : Got not the special object but a matrix
# >
#
