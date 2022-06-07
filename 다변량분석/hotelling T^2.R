# hotelling T^2
# 다변량 표본
# 한 개 모집단, 서로 독립, 모평균 벡터mu, 공분산행렬이 sigma인 p-변량
# 정규분포를 따르는 n개의 px1확률 벡터 x_1,...,x_n을 확률표본으로 얻은 경우
df = read.csv('./data/sweat.csv')

df = df[, c(2:4)]

apply(df, 2, mean) # 표본평균벡터

cov(df) #표본공분산행렬렬

S_inverse = solve(cov(df)) # S^-1 (역행렬)

mu = matrix(c(4,50,10), ncol = 1)
x_bar = matrix(colMeans(df), ncol = 1)
n = nrow(df)
p = ncol(df)
# t^2 통계량
T_sq = n*t(x_bar-mu)%*%S_inverse%*%(x_bar-mu)

(n-1)*p / (n-p)
qf(0.95,p,n-p) # 
(n-1)*p / (n-p) * qf(0.95,p,n-p)
# t^2이 10.73보다 크지않으므로 h0기각 X
