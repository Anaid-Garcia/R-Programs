# Anaid Garcia
# Project 6
# 12/11/20

task1 <- function(R, C){
#DATA 1
data1 <- c(1,2,"dog", TRUE, FALSE, "fish")
m1 <- matrix(data1, ncol = 3, nrow = 2)
print(m1)

print(paste("Type:", typeof(m1)))
print(class(m1))

#DATA 2
data2 <- c(1,2,16, TRUE, FALSE, TRUE)
m2 <- matrix(data2, ncol = 3, nrow = 2)
print(m2)

print(paste("Type:", typeof(m2)))
print(class(m2))

#DATA 3
data3 <- c(1:(R*C))
m3 <- matrix(data3, ncol = C, nrow = R)
print(m3)

#DATA 3
m4 <- matrix(data3, ncol = C, nrow = R, byrow = TRUE)
print(m4)
}

task2 <- function(R,C){
  values <- 1:(R*C)
  mat <- matrix(values, ncol = C, nrow = R, byrow = TRUE)
  print(mat)
  image(x=1:R, y=1:C, z=mat)
  print(t(mat))
  image(x=1:C, y=1:R, z=t(mat))
  image(x=1:C, y=1:R, z=t(mat), xaxt='n', yaxt='n')
  axis(side = 1, at= 1:C, labels = 1:C)
  axis(side = 2, at= 1:R, labels = 1:R)
}
f <- function(x, y){
  return( (x*y) + (2*y) )
}
task3 <- function(dx,dy,step){
  xseq <- seq(0,dx,step)
  yseq <- seq(0,dy,step)
  
  mat <- matrix(ncol = length(xseq), nrow = length(yseq))
  for(i in 1:length(yseq)){
    for(j in 1:length(xseq)){
      mat[i,j] <- f(xseq[j], yseq[i])
    }
  }
  print(mat)
  image(y=xseq, x=yseq, z=mat)
  image(x=xseq, y=yseq, z=t(mat))
  
  
}
task1(3,4)
task1(2,5)
task2(3,4)
task3(5,10,0.5)

