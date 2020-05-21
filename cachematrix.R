## En esta función, se presenta el operador << - que se puede utilizar para asignar 
## un valor a un objeto en un entorno que es diferente del entorno actual. A continuación 
## hay dos funciones que se utilizan para crear un objeto especial que almacena una matriz 
## invertible y la inversa en la memoria caché.

# Esta función realiza la creación del objeto especial y la segunda evalua la existencia de 
# inversa de la respectiva matriz, ya que si esta ya existe se ahorra procesamiento

#esta función crea un objeto especial "matriz" que puede almacenar en caché su inverso.
#Ahorra procesamiento y calcular repetidas veces la inversa de la matriz de forma innecesaria
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # Objeto que tendrá la inversa de la matriz
  
  #Creación de un objeto especial
  
  # Establecer el valor de la matriz
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  # Obtener el valor de la matriz
  get <- function(){
    x
  }
  #Estableciendo el valor de la inversa
  setsolve <- function(solve){
    m <<- solve
  }
  getsolve <- function(){
    m
  }
  
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
  
}


## Esta función calcula el inverso de la "matriz" especial devuelta por makeCacheMatrix arriba. 
##Si ya se ha calculado el inverso (y la matriz no ha cambiado), entonces el caché debe 
##recuperar el inverso del caché

cacheSolve <- function(x, ...) {
  ## Retorna una matriz que es la inversa de "x"
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
