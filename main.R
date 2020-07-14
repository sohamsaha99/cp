# library(png)
# library(jpeg)
library(plot3D)
f = function(x, y)
{
    -2*(x-0.5)^2 - 2*(y-0.5)^2 + as.numeric( (x-0.5)^2 + (y-0.5)^2 <= 0.25*0.25)
}
x = y = 0:24 / 24
z0 = outer(x, y, f)
z = z0 + rnorm(625, 0, 3.0)
persp3D(z = z)
triangular_1d = function(x)
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
npr = function(x0, y0, x, y, z, h = 0.5, ker = kernel)
{
    wt = ker((x - x0)/h, (y - y0)/h)
    return(sum(wt * z)/sum(wt))
}
print(npr(x[1], y[1], x, y, z))
npr_2d = function(x0, y0, x, y, z, h = 0.5, ker = kernel)
{
    mapply(npr, x0, y0, MoreArgs = list(x=x, y=y, z=z, h = h, ker = ker))
}


h = seq(0.10, 2.0, by=0.05)
mse = NULL
for(k in 1:length(h))
{
    Z = outer(x, y, npr_2d, x=x, y=y, z=z, h=h[k])
    mse[k] = mean((z0-Z)^2)
}
plot(h, mse)
print(h[which.min(mse)])
abline(v=h[which.min(mse)])

Z = outer(x, y, npr_2d, x=x, y=y, z=z, h=h[which.min(mse)])

persp3D(z = Z)
print(1)


z = z0 + rnorm(625, 0, 0.5)
persp3D(z = z)
lpr = function(x0, y0, x, y, z, h = 0.5, ker = kernel)
{
    wt = ker((x - x0)/h, (y - y0)/h)
    # X = replicate(length(y), x)
    # Y = replicate(length(x), y)
    # Y = diag(y)
    wt_x = wt * x
    wt_y = wt * rep(y, each = length(x))
    sum_wt_z = sum(wt * z)
    sum_wt_xy = sum(wt_y * x)
    sum_wt_xz = sum(wt_x * z)
    sum_wt_yz = sum(wt_y * z)
    W = matrix(NA, nrow=3, ncol=3)
    W[1,1] = sum(wt)
    W[1, 2] = W[2, 1] = sum(wt_x)
    W[1, 3] = W[3, 1] = sum(wt_y)
    W[2, 3] = W[3, 2] = sum_wt_xy
    W[2, 2] = sum(wt_x * x)
    W[3, 3] = sum(wt_y * rep(y, each = length(x)))
    W = solve(W)
    c(1, x0, y0) %*% W %*% c(sum_wt_z, sum_wt_xz, sum_wt_yz)
}
print(lpr(x[1], y[1], x, y, z))

lpr_2d = function(x0, y0, x, y, z, h = 0.5, ker = kernel)
{
    mapply(lpr, x0, y0, MoreArgs = list(x=x, y=y, z=z, h = h, ker = ker))
}
Z = outer(x, y, lpr_2d, x=x, y=y, z=z, h=0.35)
persp3D(z = Z)

h = seq(0.05, 1.0, by=0.05)
mse = NULL
for(k in 1:length(h))
{
    Z = outer(x, y, lpr_2d, x=x, y=y, z=z, h=h[k])
    mse[k] = mean((z0-Z)^2)
}
plot(h, mse)
print(h[which.min(mse)])
abline(v=h[which.min(mse)])

Z = outer(x, y, lpr_2d, x=x, y=y, z=z, h=h[which.min(mse)])

persp3D(z = Z)
print(1)