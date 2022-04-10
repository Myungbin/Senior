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
