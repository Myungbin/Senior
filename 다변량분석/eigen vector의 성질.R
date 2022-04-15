A = matrix(c(3,1, 1,3), nc = 2)

eigen(A) 
# eigen() decomposition
# $values
# [1] 4 2
# 
# $vectors
# [,1]       [,2]
# [1,] 0.7071068 -0.7071068
# [2,] 0.7071068  0.7071068
trace = sum(diag(A))
trace
# 고유값의 합은 tr(A)와 같다
det(A)
# 고유값의 곱은 det(A)와 같다