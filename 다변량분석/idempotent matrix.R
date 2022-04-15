############# idempotent matrix ############# 
n <- 3
In <- diag(rep(1,n))
Jn <- matrix(rep(1, n*n), ncol=n)
In
Jn

A <- In - Jn/n
A
A %*% A
