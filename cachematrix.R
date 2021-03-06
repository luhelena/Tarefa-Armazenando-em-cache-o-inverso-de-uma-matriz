## Esta fun��o cria um objeto "matriz" especial que pode armazenar em cache seu inverso.


makeCacheMatrix <- fun��o (x = matrix ()) {
  i <- NULL
  set <- fun��o (y) {
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

## Essa fun��o calcula o inverso da "matriz" especial retornada pelo makeCacheMatrix acima. Se o inverso j� tiver sido calculado (e a matriz n�o tiver sido alterada), 
## o cachesolve dever� recuperar o inverso do cache.

cacheSolve <- fun��o (x, ...) {
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
