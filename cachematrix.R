## The makeCacheMatrix will recive an matrix can can be inversed
## The makeCacheMatrix will be saved on a variable (we can called it 'x')
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## cacheSolve will calculate and return the inverse of x
cacheSolve <- function(x,...){
  ##Here, the function will get the value of x, and will check that is the first time you introduce x
  ##If x was already calculated the fuction will return the avaiable value 
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##if x is a new value, the fuction will calculate it inverse with solve()
  inv_matrix <- x$get()
  m <- solve(inv_matrix,...)
  x$set_inverse(m)
  m
}
