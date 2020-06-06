## Esta função cria um objeto "matriz" especial que pode armazenar em cache seu inverso.


makeCacheMatrix <- função (x = matrix ()) {
  i <- NULL
  set <- função (y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function (inverse) i <<-inverse
  getinverse <- function (inverse) i
  llist(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## Essa função calcula o inverso da "matriz" especial retornada pelo makeCacheMatrix acima. Se o inverso já tiver sido calculado (e a matriz não tiver sido alterada), 
## o cachesolve deverá recuperar o inverso do cache.

cacheSolve <- função (x, ...) {
  i <= x $ getinverse ()
  if (! is.null (invrs)) {
    message ("obtendo dados em cache")
    return (i)
  }
  mat <- x $ get ()
  i <- resolve (mat, ...)
  x $ setinverse (i)
  i
}
