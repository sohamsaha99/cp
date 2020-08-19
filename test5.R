set.seed(1702)
x=y=0:99/100
f = function(x, y)
{
    -2*(x-0.5)^2 - 2*(y-0.5)^2 + as.numeric( (x-0.5)^2 + (y-0.5)^2 <= 0.25*0.25)
}
z0 = outer(x, y, f)
z = z0 + rnorm(nrow(z0)*ncol(z0), 0, 0.1)
x0 = 33
y0 = 71

library(rgl)
library(plot3D)
X=mesh(x,y)$x
Y=mesh(x,y)$y
colmat = z*0+8
colmat[27:39, 65:77] = 2
colmat[x0, y0] = 4
# plot3d(x=X, y=Y, z=z, axes=TRUE, xlab="x", ylab="y", zlab="z", col = colmat, size = 3)
# view3d(0,0)
x=X[-6:6+x0, -6:6+y0]
y=Y[-6:6+x0, -6:6+y0]
z=z[-6:6+x0, -6:6+y0]
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(z, nbcol)
plot3d(x=x, y=y, z=z, axes=TRUE, xlab="x", ylab="y", zlab="z", col = color[zcol], size = 6, main="13x13 grid")
# view3d(0, 0)

xv = as.vector(x)
yv = as.vector(y)
zv = as.vector(z)
model = lm(zv~xv+yv)
a=model$coe[2]; b=model$coe[3]; c=-1; d=model$coe[1]
planes3d(a=a,b=b,c=c,d=d, alpha=0.2)
# aspect3d("iso")
# highlevel()

p0 = c(0.32, 0.70, a*0.32+b*0.70+d)
# arrow3d(p0 = p0, p1 = p0 - 0.007*c(a, b, c), type = "line", col = "red", lwd=3, theta=pi/24)
# arrow3d(p0 = p0, p1 = p0 + 0.007*c(a, b, c), type = "line", col = "red", lwd=3, theta = pi/24)
# rgl.abclines(p0[1], p0[2], p0[3], a, b, c, lwd=3, col="red")

p0 = c(0.32, 0.70, min(zv))
# arrow3d(p0 = p0, p1 = p0 - 0.007*c(a, b, 0), type = "line", col = "red", lwd=3, theta=pi/24)
# arrow3d(p0 = p0, p1 = p0 + 0.007*c(a, b, 0), type = "line", col = "red", lwd=3, theta = pi/24)
# rgl.abclines(p0[1], p0[2], p0[3], a, b, 0, lwd=3, col="red")

p0 = c(0.32, 0.70, min(zv))
rgl.abclines(p0[1], p0[2], p0[3], -b, a, 0, lwd=3, col="blue", lty=1)
p0 = c(0.34, 0.69, min(zv))
abclines3d(p0[1], p0[2], p0[3], -b, a, 0, lwd=3, col="black", lty=2)
p0 = c(0.30, 0.72, min(zv))
rgl.abclines(p0[1], p0[2], p0[3], -b, a, 0, lwd=3, col="green", lty=3)

view3d(0,0)
print(c(a,b,c,d))