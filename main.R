# library(png)
# library(jpeg)
library(plot3D)
f = function(x, y)
{
    -2*(x-0.5)^2 - 2*(y-0.5)^2 + as.numeric( (x-0.5)^2 + (y-0.5)^2 <= 0.25*0.25)
}
x = y = 0:24 / 24
z = outer(x, y, f) + rnorm(625)
persp3D(z = z)
triangular_1d = function(x, y)
{
    (1 - abs(x)) * as.numeric(abs(x) <= 1)
}
triangular_2d = function(x, y)
{
    (1 - abs(x)) * as.numeric(abs(x) <= 1) * (1 - abs(y)) * as.numeric(abs(y) <= 1)

}
kernel = function(x, y)
{
    outer(x, y, triangular_2d)
}
npr = function(x0, y0, x, y, z, h = 0.5, ker = triangular)
{
    wt = kernel((x - x0)/h, (y - y0)/h)
    return(sum(wt * z)/sum(wt))
}
print(npr(x[1], y[1], x, y, z))
npr_2d = function(x0, y0, h)
{
    mapply(npr, x0, y0, MoreArgs = list(x=x, y=y, z=z, h = h))
}

Z = outer(x, y, npr_2d, h=0.5)

persp3D(z = Z)

h = seq(0.05, 2, by=0.05)
mse = NULL
for(k in 1:length(h))
{
    Z = outer(x, y, npr_2d, h[k])
    mse[k] = mean((z-Z)^2)
}
plot(h, mse)