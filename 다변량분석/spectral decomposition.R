A <- matrix(c(3,1, 1, 3), ncol=2, byrow=T)  
egnA <- eigen(A)
egnval <- egnA$values
egnvec <- egnA$vectors

sum(diag(A))  # trace
sum(egnval)   
det(A)        # determinant
prod(egnval)  

############## Spectral decomposition ############## 

egnvec %*% t(egnvec) #pxp' = I
t(egnvec) %*% egnvec #p'xp = I

egnval[1] * egnvec[,1] %*% t(egnvec[,1]) +
    egnval[2] * egnvec[,2] %*% t(egnvec[,2])
# lambda_1 x e_1 x e_1' + lambda_2 x e_2 x e_2' = A

egnvec %*% diag(egnval) %*% t(egnvec)
# P x lambda x P'

solve(A)  # 역행렬

egnvec %*% diag(1/egnval) %*% t(egnvec)
# P x lambda^-1 x P' => 역행렬
