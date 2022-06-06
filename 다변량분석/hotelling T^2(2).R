# 다변량 표본이며 sigma_1=sigma_2=sigma인 경우
# sigma는 알려져 있지 않다고 가정

library(dplyr)
# 성별에 따른 평균벡터간 차이가 있는지 검정
# 각 집단은 공분산행렬이 같은 다변량 정규분포를 따른다고 가정
df = read.csv('./data/pschy.csv')
df

df_m = filter(df, gender == 1)
df_m = df_m[, c(2:5)]
df_m

df_w = filter(df, gender == 2)
df_w = df_w[, c(2:5)]
df_w
