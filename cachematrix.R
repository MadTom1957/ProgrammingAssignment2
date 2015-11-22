## This function produces a list that is used by cacheSolve() to eliminate 
## the need for calculating an inverse to the same matrix more than once. 
## This function calculates the inverse only once and stores the value to be
## recalled by cacheSolve()
  
makeCacheMatrix <- function(A = matrix())
{
	## The final list will contain four items:
	## 1)	set = stores the value of matrix and the flag "m" in the parent environment 
	## 2)	get = is the original matrix A
	## 3)	setinverse contains the solve function
	## 4)	getinverse holds the calculated inverse

	m <- NULL
	set <- function(B)
	{
		A <<- B
		m <<- NULL
	}
	get <- function() A
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set , get = get , setinverse = setinverse , getinverse = getinverse )
}


## This function checks to see if an inverse has been calculated and cached by
## a previous function, "makeCacheMatrix()".  If the flag "m" is null, it 
## calculates the inverse. If the flag is not NULL, it uses the stored cached
## inverse.

cacheSolve <- function(A, ...)
{
	m <- A$getinverse()
	if(!is.null(m))
	{
		message("...using cached inverse...")
		return(m)
	}
	my_matrix <- A$get()
	m <- solve(my_matrix, ...)
	A$setinverse(m)
	m
}
