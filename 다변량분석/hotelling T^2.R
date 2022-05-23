# hotelling T^2
df = read.csv('./data/sweat.csv')

df = df[, c(2:4)]

apply(df, 2, mean) # 표본평균벡터

cov(df) #표본공분산행렬렬

S_inverse = solve(cov(df)) # S^-1 (역행렬)

mu = matrix(c(4,50,10), ncol = 1)
x_bar = matrix(colMeans(df), ncol = 1)
n = nrow(df)

# t^2 통계량
T_sq = n*t(x_bar-mu)%*%S_inverse%*%(x_bar-mu)


