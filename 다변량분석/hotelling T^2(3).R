library(ICSNP)

df <- read.csv('./data/sweat.csv', header = T)
df

xx <- df[, -1]  # 1열(id)제외
mu0 <- c(4, 50, 10) # 검정값

HotellingsT2(xx, mu = mu0, test = 'f')

#################################################################

nn <- dim(xx)[1]; pp <- dim(xx)[2]
xbar <- apply(xx, 2, mean)
S <- var(xx)
xbar; S

T2 <- nn*t(x_bar-mu0) %*% solve(S) %*% (x_bar-mu0)
T2

F0 <- T2 * (nn-pp) / ((nn-1)*pp)

1-pf(F0, pp, nn-pp)

#################################################################

# 이표본
pschy <- read.csv('./data/pschy.csv', header = T)

grp <- factor(pschy[,1])
xx <- pschy[, -1]
HotellingsT2(as.matrix(xx)~grp)

#################################################################
# 성별에 따른 평균벡터간 차이가 있는지 검정
# 각 집단은 공분산행렬이 같은 다변량 정규분포를 따른다고 가정
library(dplyr)
df = read.csv('./data/pschy.csv')
df
pp <- dim(xx)[2]
df_m = filter(df, gender == 1)
df_m = df_m[, c(2:5)]
df_m

df_w = filter(df, gender == 2)
df_w = df_w[, c(2:5)]
df_w

n1 <- dim(df_m)[1]
n2 <- dim(df_w)[1]
c(n1, n2)

x1bar <- apply(df_m, 2, mean)
x2bar <- apply(df_w, 2, mean)
c(x1bar, x2bar)

S1 <- cov(df_m)
S2 <- cov(df_w)

Sp <- ((n1-1)*S1 + (n2-1)*S2) / (n1+n2-2)

T2p <- t(x1bar-x2bar) %*% solve(Sp*(1/n1+1/n2)) %*% (x1bar-x2bar)
T2p

F0p <- T2p *(n1+n2-pp-1)/((n1+n2-2)*pp)
F0p

1-pf(F0p, pp, n1+n2-pp-1)

#################################################################
# 이표본 보기 
# 두 그룹의 분산공분산행렬이 같지않을떄(데이터 개수가 다름)
azoto <- read.table('./data/Azotobacter.txt', sep = ",", header = T)

xx <- azoto[, -1]
pp <- dim(xx)[2]
x1 = azoto[azoto[,1]==1, -1]
x2 = azoto[azoto[,1]==2, -1]

n1 <- dim(x1)[1]
n2 <- dim(x2)[1]
c(n1, n2)

x1bar <- apply(x1, 2, mean)
x2bar <- apply(x2, 2, mean)

S1 <- cov(x1)
S2 <- cov(x2)

T2 <- t(x1bar-x2bar) %*% solve(S1/n1 + S2/n2) %*% (x1bar-x2bar)

1-pchisq(T2, pp)
# 