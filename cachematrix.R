
# Step 1 Matrix Inverse

#This function creates a special "matrix" object that can cache its inverse.

makeMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

# test

my_matrix <- makeMatrix(matrix(1:4, 2, 2))
my_matrix$get()

my_matrix$getInverse()

cacheSolve(my_matrix)

cacheSolve(my_matrix)

my_matrix$getInverse()
