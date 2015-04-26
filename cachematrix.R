makeCacheMatrix<-function(b=matrix()) {
  #this function is a version of makeVector to inverse matrices.
  #m1 will be null until receiving a matrix, when a matrix enters as an argument, it will be inverted.
  #by cacheSolve. get and set will help to check whether an inverse value can be found in the function environment.
  m1<-NULL
  set <-function(n1){
    b<<-n1
    m1<<-NULL    
  }
  get<-function() b
  setinverse<-function(inverse) m1<<-inverse
  getinverse<-function() m1
  
  list(set=set,get=get,getinverse=getinverse,setinverse=setinverse,cacheSolve=cacheSolve)
  
}

cacheSolve<-function(b,...){
  
  #cacheSolve checks for m1 where "data" is the cached value of m1 inversed.
  # if not found in cached data, it will conduct solve on the matrix to get inverse
  m1<-b$getinverse()
  if(!is.null(m1)) {
    message("getting cached data")  
    return(m1)
    
  }
  data<-b$get()
  m1<-solve(data,...)
  b$setinverse(m1)
  m1    
}

testcache<-function() {
  #this function helped me test what values will appear for d in different environments
  d<<-3.14
  print(d)
  print(d)
  d<-42
  print(d)
  
  print(d)
}


plusFunctions<-function() {
  plustwo<-function(y) {
    x<-y+2
    return(x)
  }
  
  plusthree<-function(y) {
    x<-y+3
    return(x)
    
  }  
  list(plustwo=plustwo, plusthree=plusthree)
} 

