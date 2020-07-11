## Put comments here that give an overall description of what your
## functions do

## WEsta funcion crea un elemento "makecachematrix", el cual devuelve un elemento que dispone de funciones

makecachematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Esta funcion recibe un elemento ripo "makecachematrix" y retorna su valor inverso
## de igual manera cuenta con las funciones que le permiten obtener y definir las entradas a la funcion

cachesolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
  } 