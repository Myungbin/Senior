help(CO2)

CO2 = CO2

CO2chilled = CO2[CO2[,3] == 'chilled', ]
CO2chilled = CO2chilled[, 4:5]


S <- cov(CO2chilled)
d <- mahalanobis(CO2chilled, colMeans(CO2chilled), S) # Mahalanobis distance 
d2 <- sort(d)



## 문제 2 ####

library(ICSNP)
mu = matrix(c(450, 25), ncol = 1)
HotellingsT2(CO2chilled, mu = mu, test = 'f')



x_bar = apply(CO2chilled, 2, mean)
S = var(CO2chilled)
S_inv = solve(S)
n = nrow(CO2chilled); n
p = ncol(CO2chilled); p

T_sq = n*t(x_bar - mu) %*% S_inv %*% (x_bar - mu)

F0p = T_sq * (n-p) / ((n-1)*p)

1-pf(F0p, p, n-p)


## 문제 3번 ####

grp = factor(CO2[,3])
xx = CO2[,4:5]
HotellingsT2(as.matrix(xx) ~ grp)

X1 = CO2[CO2$Treatment == 'chilled', ]
X1 = X1[, c(4:5)]
X2 = CO2[CO2$Treatment == 'nonchilled', ]
X2 = X2[, c(4:5)]

X1_bar = apply(X1, 2, mean)
X1_S = var(X1)
X2_bar = apply(X2, 2, mean)
X2_S = var(X2)

n1 = nrow(X1); n1
n2 = nrow(X2); n2
p = ncol(X1); p

Spl = ((n1-1)*X1_S + (n2-1)*X2_S) / (n1+n2-2)

T_sq = t(X1_bar - X2_bar) %*% solve(Spl*(1/n1+1/n2)) %*% (X1_bar - X2_bar)

F0p = T_sq*(n1+n2-p-1) / ((n1+n2-2)*p)

1-pf(F0p, p, n1+n2-p-1)


## 문제 4번 ##


X1 = CO2[CO2$Treatment == 'chilled', ]
X1 = X1[, c(4:5)]
X2 = CO2[CO2$Treatment == 'nonchilled', ]
X2 = X2[, c(4:5)]

X1_bar = apply(X1, 2, mean)
X1_S = var(X1)
X2_bar = apply(X2, 2, mean)
X2_S = var(X2)

n1 = nrow(X1); n1
n2 = nrow(X2); n2
p = ncol(X1); p

T_sq = t(X1_bar - X2_bar) %*% solve(X1_S/n1 + X2_S/n2) %*% (X1_bar - X2_bar)

1-pchisq(T_sq, p)
