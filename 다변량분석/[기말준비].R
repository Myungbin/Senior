# 기말고사 연습

##### 5.1 ###################################################################
mat =  matrix(c(2, 8, 6, 8, 12, 9, 9, 10), nrow = 4)

# 5.1.a) 표본평균벡터를 구하세요
x_bar = apply(mat, 2, mean)

# 5.1.b) 표본공분산행렬 S를 구하세요
S = var(mat)

# 5.1.c) S^-1를 구하세요
S_inv = solve(S)

# 5.1.d) H0: mu = (7, 11) 에 대해 유의수준 0.05에서 양측검정(hotelling t^2이용)

## 방법1
library(ICSNP)
mu = matrix(c(7, 11), ncol = 1)
HotellingsT2(mat, mu = mu, test = 'f')

## 방법2
mu = matrix(c(7, 11), ncol = 1)
n = nrow(mat); n
p = ncol(mat); p

T_sq = n*t(x_bar - mu) %*% S_inv %*% (x_bar - mu)

F0p = T_sq * (n-p) / ((n-1)*p)

1-pf(F0p, p, n-p)

# 5.1.e) CX_i를 변수들의 선형식으로 표현

C = matrix(c(1,1,-1,1), ncol = 2)

# 5.1.f) C를 통해 변환된 데이터를 계산
X_i1 = mat[,1] - mat[,2]
X_i2 = mat[,1] + mat[,2]
CX = cbind(X_i1, X_i2)

# 5.1.g) CX와 Cmu를 이용하여 hotelling T^2통계량 d값과 일치하는지 비교
CS = cov(CX)
CS_inv = solve(CS)
C_mu = C %*% mu; C_mu
Cn = ncol(CX); Cn
Cp = nrow(CX); Cp
CX_bar = apply(CX, 2, mean); CX_bar

CT_sq = n*t(CX_bar - C_mu) %*% CS_inv %*% (CX_bar - C_mu)

# HotellingsT2(CX, mu = C_mu, test = 'f') 방법
# 4.5455*((n-1)*p)/(n-p)

# 5.1.h) 모평균벡터 mu에대한 95% 동시신뢰영역을 구하고 그래프로 그리기
library(PlaneGeometry)

Sinv = S_inv/14.25
mvec = matrix(c(6,10), ncol = 1) # 평균벡터 x_bar

ell <- EllipseFromCenterAndMatrix(mvec, Sinv)

box = ell$boundingbox()

plot(NULL, xlim = box$x, ylim = box$y, xlab = NA, ylab = NA)
draw(ell, col = "yellow", border = "blue", lwd = 2)

# 모수에 대한 신뢰구간을 따로 구하여 교집합으로 구하기
qt(0.9875, 3) #t_3(0.0125)
x1_up = x_bar[1]+qt(0.9875, n-1)*sqrt(S[1,1]/n)
x1_down = x_bar[1]-qt(0.9875, n-1)*sqrt(S[1,1]/n)

x2_up = x_bar[2] + qt(0.9875, n-1)*sqrt(S[2,2]/n)
x2_down = x_bar[2] - qt(0.9875, n-1)*sqrt(S[2,2]/n)



rect(xleft=x1_down, ybottom=x2_down, xright=x1_up, ytop=x2_up, 
     lwd=2, border="red")

points(6, 10, pch=15)

# 5.1.i) 두 개체 간에 유클리드 거리
dist <- dist(mat, method="euclidean") # method 생략가능
dist

###### 5.4 ###################################################################
# H0: mu = (6, 11) H1: mu != (6, 11)
library(ICSNP)
mat =  matrix(c(3,6,5,10,10,12,14,9), nrow = 4)
mu = matrix(c(6,11), ncol = 1)
HotellingsT2(mat, mu = mu, test = 'f')

###### 5.5 ###################################################################

# 5.5.a) 두집단 각각의 표본평균벡터와 표본공분산 행렬
df = read.csv('./data/job.csv')

A = df[df$job == 1, ]
A = A[, c(2:3)]; A

B = df[df$job == 2, ]
B = B[, c(2:3)]; B

A_bar = apply(A, 2, mean)
A_S = var(A)
B_bar = apply(B, 2, mean)
B_S = var(B)

# 5.5.b) 두집단의공분산 행렬이 같다고 가정
# H0: mu1 = mu2 H1: mu1 != mu2
# 방법1
library(ICSNP)
grp = factor(df[,1])
xx = df[,-1]
HotellingsT2(as.matrix(xx) ~ grp)

# 방법2
n1 = nrow(A); n1
n2 = nrow(B); n2
p = ncol(xx); p

Spl = ((n1-1)*A_S + (n2-1)*B_S) / (n1+n2-2)

T_sq = t(A_bar - B_bar) %*% solve(Spl*(1/n1+1/n2)) %*% (A_bar - B_bar)

F0p = T_sq*(n1+n2-p-1) / ((n1+n2-2)*p)

1-pf(F0p, p, n1+n2-p-1)

# 5.5.c) 두집단의 공분산행렬이 같지않다고 가정
# H0: mu1 = mu2 H1: mu1 != mu2
T_sq = t(A_bar - B_bar) %*% solve(A_S/n1 + B_S/n2) %*% (A_bar - B_bar)

1-pchisq(T_sq, p)

#### 짝 검정 ##############################################################

pipe = read.csv('./data/pipe.csv')

A = pipe[pipe$코팅 == 'A', ]
A = A[, c(2:3)]

B = pipe[pipe$코팅 == 'B', ]
B = B[, c(2:3)]

d1 = A[1] - B[1]
d2 = A[2] - B[2]
df = cbind(d1, d2)

d_bar = apply(df, 2, mean)
S_d = cov(df)
n = nrow(df)
p = ncol(df)

T_sq = t(d_bar) %*% solve(S_d/n) %*% (d_bar)

f0p = T_sq * (n-p) / (n-1)*p

1-pf(f0p, p, n-p)
