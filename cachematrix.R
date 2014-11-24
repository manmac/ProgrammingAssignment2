## Vamos a calcular funciones que capturen la inversa de una matriz

## Creamos una matriz para guardar la inversa
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initializamos i a NULL
  i <- NULL
  
  ## Creamos un método que cree la matriz
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Ahora hacemos un get de la matriz
  get <- function() {
    
    ## Devolvemos la matriz
    m
  }
  
  ## Calculamos la inversa de la matriz
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Obtenemos la inversa
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  ## Devolvemos una lista de métodos
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Calculamos la inversa devuelta por "makeCacheMatrix"
cacheSolve <- function(x, ...) {
  
  ## Devolvemos una matriz que es la inversa de x
  m <- x$getInverse()
  
  ## Devolvemos la inversa
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Capturamos la matriz de nuestro objeto ...
  data <- x$get()
  
  ## Ahora la inversa usando la matriz multiplicación
  m <- solve(data) %*% data
  
  ## Establecemos la inversa
  x$setInverse(m)
  
  ## Devolvemos la matriz
  m
}