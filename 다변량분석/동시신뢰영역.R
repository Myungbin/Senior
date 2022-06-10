library(PlaneGeometry)

# 분산공분산행렬(S)의 역함수
Sinv <- matrix(c(0.421, -0.021, -0.021, 0.005), ncol=2) *20/7.49 
# 평균벡터
mvec <- c(4.64, 45.4)

ell <- EllipseFromCenterAndMatrix(mvec, Sinv)

box <- ell$boundingbox()

#xlim, ylim x,y 범위, asp 
plot(NULL, asp = 0.1, xlim = box$x, ylim = box$y, xlab = NA, ylab = NA)

draw(ell, col = "yellow", border = "blue", lwd = 2)

rect(xleft=3.717, ybottom=37.71, xright=5.563, ytop=53.09, 
     lwd=2, border="red")

points(4.64, 45.4, pch=15)
