# basicstats1.r
x1 <- c(90, 80, 75, 70, 65)
x2 <- c(80, 90, 80, 70, 80)

x <- cbind(x1, x2) # or
x <- data.frame(x1=x1, x2=x2)

x

S = var(x)   # or cov(x)
S

g_var <- det(S)
g_var


total_var1 <- var(x1) + var(x2)
# total_var2 <- tr(S) # psych 
total_var3 <- sum(diag(S)) 

c(total_var1, total_var3)

R <- cor(x)
round(R, 3)
eigx <- eigen(R)
eigx

cond_num <- max(eigx$values)/min(eigx$values)
cond_num