## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
  mat <- NULL#mat will store our inv matrix and its reset to NULL 
  #every time makeCacheMatrix is called
  set <- function(y) { #takes an input vector
    x <<- y #saves the input vector
    mat <<- NULL # # resets the mean to NULL, basically what 
    #happens when a new object is generated.
  }
  get <- function() x # this will return the cached value to cacheSolve() on
  #  subsequent accesses
  setmat <- function(minv) mat <<- minv # this is called by cacheSolve() during the 
  #first cacheSolve access and it will store the value using superassignment
  getmat <- function() mat# this will return the cached value to cachemean() on
  #  subsequent accesses
  list(set = set, get = get, #  This list is returned with the newly created object.
       setmat = setmat, #   It lists all the functions that are part of
       getmat = getmat)#   the object.  If a function is not on the list then it cannot
  #   be accessed externally.
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { # the input is an object created by cacheSolve
  mat <- x$getmat()  # accesses the object 'x' and gets the value of the mean
  if(!is.null(mat)) {  # if mean was already cached (not NULL) ...
    message("getting cached data")  # ... send this message to the console
    return(mat) # ... and return the mean ... "return" ends  
    #   the function cachemean(), note
  }
  data <- x$get() # we reach this code only if x$getmat() returned NULL
  mat <- solve(data, ...) # if mat was NULL then we have to calculate the mean
  x$setmat(mat) # store the calculated mean value in x (see setmat() in makeCacheMatrix)
  mat  # return the mean to the code that called this function
}
