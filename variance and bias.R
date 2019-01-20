####################################
# # Project: Experiments-with-bias-and-variance
# # Author: Simran Tinani
# # Date: 20-01-2019
# # Remarks: Inspired by the problems of homework assignment 4 of the Learning from Data telecourse 
#     by Yaser Abu-Mostafa (CalTech University): https://work.caltech.edu/homework/hw4.pdf
#####################################

# hypotheses: lines through origin 
f <- function(x){ # target function
  sin(pi*x)
}
a_D = 0
v = matrix(nrow = 1000, ncol = 1000)
b = matrix(nrow = 1000, ncol = 1000)
z = 0
var = matrix(nrow =1, ncol = 1000) 
bias = matrix(nrow = 1, ncol =1000)
var[1, 1:1000] = 0
bias[1, 1:1000] = 0

j = 1
i = 1
while(j<1001){
  z[j] = runif(1, -1, 1)
  i = 1
  while(i<1001){
X = runif(2, -1, 1)
D=matrix(nrow = 2, ncol = 2)
D[1, ] = c(X[1], f(X[1])) #training point 1
D[2, ] = c(X[2], f(X[2])) #training point 2
x1 = D[1,1]
x2 = D[2,1]
y1 = D[1,2]
y2 = D[2,2] 
a_D[i] = (x1*y1 + x2*y2)/(x1^2 + x2^2)
v[i,j] = (a_D[i]*z[j] - 1.42*z[j])^2
var[1, j] = var[1, j] + v[i,j]/1000
  b[i,j] = (1.42*z[j] - f(z[j]))^2
  bias[1, j] = bias[1, j] + b[i,j]/1000
  i = i+1
  }
j = j + 1
}
mean(a_D)
mean(var)
mean(bias)






#constants

f <- function(x){
  sin(pi*x)
}
a_D = 0
v = matrix(nrow = 1000, ncol = 1000)
b = matrix(nrow = 1000, ncol = 1000)
z = 0
var = matrix(nrow =1, ncol = 1000) 
bias = matrix(nrow = 1, ncol =1000)
var[1, 1:1000] = 0
bias[1, 1:1000] = 0

j = 1
i = 1
while(j<1001){
  z[j] = runif(1, -1, 1)
  i = 1
  while(i<1001){
    X = runif(2, -1, 1)
    D=matrix(nrow = 2, ncol = 2)
    D[1, ] = c(X[1], f(X[1]))
    D[2, ] = c(X[2], f(X[2]))
    x1 = D[1,1]
    x2 = D[2,1]
    y1 = D[1,2]
    y2 = D[2,2] 
    a_D[i] = (y1 + y2)/2
    v[i,j] = (a_D[i] - -0.01407176)^2
    var[1, j] = var[1, j] + v[i,j]/1000
    b[i,j] = (-0.01407176 - f(z[j]))^2
    bias[1, j] = bias[1, j] + b[i,j]/1000
    i = i+1
  }
  j = j + 1
}
mean(a_D)
mean(var)
mean(bias)

#lines

f <- function(x){
  sin(pi*x)
}
a_D = 0
v = matrix(nrow = 1000, ncol = 1000)
b = matrix(nrow = 1000, ncol = 1000)
z = 0
var = matrix(nrow =1, ncol = 1000) 
bias = matrix(nrow = 1, ncol =1000)
var[1, 1:1000] = 0
bias[1, 1:1000] = 0
a = 0
d = 0
j = 1
i = 1
while(j<1001){
  z[j] = runif(1, -1, 1)
  i = 1
  while(i<1001){
    X = runif(2, -1, 1)
    D=matrix(nrow = 2, ncol = 2)
    D[1, ] = c(X[1], f(X[1]))
    D[2, ] = c(X[2], f(X[2]))
    x1 = D[1,1]
    x2 = D[2,1]
    y1 = D[1,2]
    y2 = D[2,2] 
    a[i]=(y2-y1)/(x2-x1)
    d[i]=(y1*x2-x1*y2)/(x2-x1)
    v[i,j] = (a[i]*z[j] + d[i] - 0.79*z[j] - -0.032)^2
    var[1, j] = var[1, j] + v[i,j]/1000
    b[i,j] = (0.84*z[j] -0.016 - f(z[j]))^2
    bias[1, j] = bias[1, j] + b[i,j]/1000
    i = i+1
  }
  j = j + 1
}
mean(a)
mean(d)
mean(var)
mean(bias)




#ax^2

f <- function(x){
  sin(pi*x)
}
a_D = 0
v = matrix(nrow = 1000, ncol = 1000)
b = matrix(nrow = 1000, ncol = 1000)
z = 0
var = matrix(nrow =1, ncol = 1000) 
bias = matrix(nrow = 1, ncol =1000)
var[1, 1:1000] = 0
bias[1, 1:1000] = 0
a = 0
d = 0
j = 1
i = 1
while(j<1001){
  z[j] = runif(1, -1, 1)
  i = 1
  while(i<1001){
    X = runif(2, -1, 1)
    D=matrix(nrow = 2, ncol = 2)
    D[1, ] = c(X[1], f(X[1]))
    D[2, ] = c(X[2], f(X[2]))
    x1 = D[1,1]
    x2 = D[2,1]
    y1 = D[1,2]
    y2 = D[2,2] 
    a[i]=(y1*x1^2 + y2*x2^2)/(x2^4 + x1^4)
    v[i,j] = (a[i]*z[j]^2 + - 0.3*z[j]^2)^2
    var[1, j] = var[1, j] + v[i,j]/1000
    b[i,j] = (0.3*z[j]^2 - f(z[j]))^2
    bias[1, j] = bias[1, j] + b[i,j]/1000
    i = i+1
  }
  j = j + 1
}
mean(a)
mean(var)
mean(bias)

#ax^2 + b

f <- function(x){
  sin(pi*x)
}
a_D = 0
v = matrix(nrow = 3000, ncol = 3000)
b = matrix(nrow = 3000, ncol = 3000)
z = 0
var = matrix(nrow =1, ncol = 3000) 
bias = matrix(nrow = 1, ncol =3000)
var[1, 1:3000] = 0
bias[1, 1:3000] = 0
a = 0
d = 0
j = 1
i = 1
while(j<3001){
  z[j] = runif(1, -1, 1)
  i = 1
  while(i<3001){
    X = runif(2, -1, 1)
    D=matrix(nrow = 2, ncol = 2)
    D[1, ] = c(X[1], f(X[1]))
    D[2, ] = c(X[2], f(X[2]))
    x1 = D[1,1]
    x2 = D[2,1]
    y1 = D[1,2]
    y2 = D[2,2] 
    d[i]=(x1^2*y2-x2^2*y1)/(x1^2-x2^2)
    a[i]=(y1-y2)/(x1^2-x2^2)
    v[i,j] = (a[i]*z[j]^2 + d[i] - 5*z[j]^2 - -1)^2
    var[1, j] = var[1, j] + v[i,j]/3000
    b[i,j] = (5*z[j]^2 + -1 - f(z[j]))^2
    bias[1, j] = bias[1, j] + b[i,j]/3000
    i = i+1
  }
  j = j + 1
}
mean(a)
mean(d)
mean(var)
mean(bias)
bias[1,2001]
bias[1,3000]
h<- function(x){
  mean(a)*x^2 + mean(d)
}
plot(h, -1, 1)
par(new=T)
plot(f, -1, 1)
var[500:1000]
var[1, 3000]
var[1101]
mean(a)
