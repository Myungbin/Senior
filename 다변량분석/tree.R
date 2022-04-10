tree<- read.csv("./data/tree.csv", header=T)
tree
cork<- tree[,2:5] # n e s w 변수
cork
plot(cork) 
m=colMeans(cork) 
m 
# N        E        S        W 
# 50.53571 46.17857 49.67857 45.21429 
cv=cov(cork)
cv
# N        E        S        W
# N 290.4061 223.7526 288.4378 225.5847
# E 223.7526 219.9299 229.0595 170.7751
# S 288.4378 229.0595 350.0040 258.9603
# W 225.5847 170.7751 258.9603 224.7672
cr=cor(cork)
cr
# N         E         S         W
# N 1.0000000 0.8853667 0.9047173 0.8829584
# E 0.8853667 1.0000000 0.8256001 0.7680969
# S 0.9047173 0.8256001 1.0000000 0.9232733
# W 0.8829584 0.7680969 0.9232733 1.0000000

library(lattice)
parallelplot(tree, main="parallel graph") # 평행 그림 
stars(cork, labels = tree[,1], main="star graph") # 별그림림
library(aplpack)
faces(cork, main="face plot for cork") 
library(lattice)
cloud(N~ E* W , data=cork) 
