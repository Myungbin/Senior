df = matrix (data = NA, nrow = 1, ncol = 1, byrow = F, dimnames = NULL)
# byrow 행으로 채울지 열로 채울지 설정
# dimnames 행과 열의 이름을 설정
df

A = matrix(1:6, nc = 3)
A

B = matrix(1:6, nr = 3)
B

C = matrix(1:6, nr = 3, byrow = T)
C

D = matrix(1:6, nr = 3, byrow = T, 
           dimnames = list(NULL, c('c1', 'c2')))
D

E = matrix(1:6, nr = 3, byrow = T, 
           dimnames = list(c('seoul', 'busan', 'daegu'), c('c1', 'c2')))
E

# 전치행렬
A = matrix(c(3, 1, -1, 5, 2, 4), nc = 3)
A

tA = t(A)
tA


a <- c(1,2,3)
b <- c(2,2,4)
a+b

t(a)*b
sum(t(a)*b)

t(a) %*% b 

3*a

la <- sqrt(t(a) %*% a ) # 유클리드 길이
la

lb <- sqrt(t(b) %*% b)
lb

cos_t <- t(a) %*% b / (la*lb)
cos_t

theta <- acos(cos_t)
theta

degrees <- theta * (180/pi)
degrees

A <- matrix(c(3,1, -1,5, 2,4), nc=3)
B <- matrix(c(1,-1, 2,3, 3,5), nc=3)
A + B
A
t(B)
A %*% t(B)


# 결정식

A = matrix(c(1, -2 , 3, 5), ncol = 2)
det(A)

A = matrix(c(1,0,1, 2,3,5, 1,4,6), ncol = 3)
det(A)

# 역행렬
# 2x2
library(MASS)
A = matrix(c(1, -3 , 2, 5), ncol = 2)
ginv(A) #방법 1 => 패키지 필요

solve(A) # 방법 2

#3x3

A = matrix(c(1,0,1, 2,3,5, 1,4,6), ncol = 3)
ginv(A)

solve(A)


A <- matrix(c(4,-5, 2,-3), ncol=2, byrow=T)
egnA <- eigen(A)

egnA$values[1]
egnA$vectors[,1]
egnA$values[2]
egnA$vectors[,2]


A <- matrix(c(3,1, 1, 3), ncol=2, byrow=T)  
egnA <- eigen(A)
egnval <- egnA$values
egnvec <- egnA$vectors

sum(diag(A))  # trace
sum(egnval)  
det(A)        # determinant
prod(egnval)  

############## Spectral decomposition

egnvec %*% t(egnvec)
t(egnvec) %*% egnvec

egnval[1] * egnvec[,1] %*% t(egnvec[,1]) +
    egnval[2] * egnvec[,2] %*% t(egnvec[,2])

egnvec %*% diag(egnval) %*% t(egnvec)

solve(A)  
egnvec %*% diag(1/egnval) %*% t(egnvec)

###

A <- matrix(c(3,1, 1, 3), ncol=2, byrow=T)  
egnA <- eigen(A)
egnval <- egnA$values
egnvec <- egnA$vectors

rootA <- egnvec %*% diag(sqrt(egnval)) %*% t(egnvec)
rootA
rootA %*% rootA
A


############# idempotent matrix
n <- 3
In <- diag(rep(1,n))
Jn <- matrix(rep(1, n*n), ncol=n)
In
Jn

A <- In - Jn/n
A
A %*% A

