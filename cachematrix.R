## Here are the functions to create a special matrix while caching it, the calculate the inverse of said matrix

## Here is the function to create a special matrix. I've added and if condition that doesn't allow for singular matrix to, 
## which is not inverasble, to be created by cheking if it's determinant equals 0

makeCacheMatrix <- function(x = matrix()) {
  
  if(det(x)!=0){
  
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)}
  
  else {stop("Determinant == 0, matrix cannot be inversed")}
}


## Here is the function that check for cashed inverse matrix and calculactes it when it's not found

cacheSolve <- function(x, ...) {
  
  s <- x$getsolve()
  
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  
  data <- x$get()
  s <- solve(data)
  x$setsolve(s)
  return(s)
}


#Testing cache
test_matrix <- makeCacheMatrix(x=matrix(c(4,5,8,9),ncol=2))

#Create cache
cacheSolve(test_matrix)

#Pull cache
cacheSolve(test_matrix)
