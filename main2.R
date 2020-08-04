# library(png)
# library(jpeg)
library(plot3D)
f = function(x, y)
{
    -2*(x-0.5)^2 - 2*(y-0.5)^2 + as.numeric( (x-0.5)^2 + (y-0.5)^2 <= 0.25*0.25)
}
x = y = 0:99 / 100
z0 = outer(x, y, f)
z = z0 + rnorm(nrow(z0)*ncol(z0), 0, 0.5)
persp3D(z = z)
print('View input. Click to continue.')
locator(1)
triangular_1d = function(x)
{
    (1 - abs(x)) * as.numeric(abs(x) <= 1)
}
triangular_2d = function(x, y)
{
    (1 - abs(x)) * as.numeric(abs(x) <= 1) * (1 - abs(y)) * as.numeric(abs(y) <= 1)

}

seperator = function(X, Y, Z) ##### X, Y vector, Z matrix len(X)xlen(Y)
{
    X = matrix(X, nrow = length(X), ncol = length(Y)); Y = matrix(rep(Y, each = nrow(X)), nrow = nrow(X))
    x = as.vector(X); y = as.vector(Y); z = as.vector(Z)
    M = matrix(0, nrow = 3, ncol = 3)
    M[1, 1] = length(x)
    M[1, 2] = M[2, 1] = sum(x)
    M[1, 3] = M[3, 1] = sum(y)
    M[2, 3] = M[3, 2] = sum(x*y)
    M[2, 2] = sum(x * x)
    M[3, 3] = sum(y * y)
    # M = solve(M)
    # # print('success')
    z0 = c(sum(z), sum(x*z), sum(y*z))
    # beta = (M %*% z0)
    beta = qr.coef(qr(M), z0); beta[is.na(beta)] = 0
    c_set = beta[2]*X + beta[3]*Y
    RSS0 = sum( (z - beta[1] - c_set)^2)
    cut = two_planes(x, y, z, beta[2:3])
    # print(cut)
    RSS = cut[2]; cut = cut[1]
    if(RSS < 0)
    {
        cut = two_planes(x, y, z, beta[2:3])
        # print(cut)
        RSS = cut[2]; cut = cut[1]
    }
    if(RSS < 0)
    {
        indicator = as.vector(c_set <= cut)
        if(sum(indicator) <= 3)
        {
            RSS1 = 0
        }
        else
        {
            xx = x[indicator]; yy = y[indicator]; zz = z[indicator]
            # print(xx); print(yy); print(zz)
            M[1, 1] = length(xx)
            M[1, 2] = M[2, 1] = sum(xx)
            M[1, 3] = M[3, 1] = sum(yy)
            M[2, 3] = M[3, 2] = sum(xx*yy)
            M[2, 2] = sum(xx * xx)
            M[3, 3] = sum(yy * yy)
            # M = solve(M)
            z0 = c(sum(zz), sum(xx*zz), sum(yy*zz))
            # beta = (M %*% z0)
            beta = qr.coef(qr(M), z0); beta[is.na(beta)] = 0
            RSS1 = sum( (zz - beta[1] - beta[2]*xx - beta[3]*yy)^2)
        }
        indicator = !indicator
        if(sum(indicator) <= 3)
        {
            RSS2 = 0
        }
        else
        {
            xx = x[indicator]; yy = y[indicator]; zz = z[indicator]
            # print(xx); print(yy); print(zz)
            M[1, 1] = length(xx)
            M[1, 2] = M[2, 1] = sum(xx)
            M[1, 3] = M[3, 1] = sum(yy)
            M[2, 3] = M[3, 2] = sum(xx*yy)
            M[2, 2] = sum(xx * xx)
            M[3, 3] = sum(yy * yy)
            # M = solve(M)
            z0 = c(sum(zz), sum(xx*zz), sum(yy*zz))
            # beta = (M %*% z0)
            beta = qr.coef(qr(M), z0); beta[is.na(beta)] = 0
            RSS2 = sum( (zz - beta[1] - beta[2]*xx - beta[3]*yy)^2)
        }
        # print(c(RSS0, RSS1, RSS2))
        RSS = RSS1 + RSS2
    }
    Fstat = ((RSS0-RSS)/3) / (RSS/(length(x)-6))
    pvalue = pf(Fstat, 3, length(x)-6, lower.tail = FALSE)
    # print(c(Fstat, pvalue, 3, length(x) - 6))
    if(pvalue > 0.00005)
    {
        return(matrix(TRUE, nrow = nrow(X), ncol = ncol(Y)))
    }
    # allocation = matrix(c_set<=cut, nrow = nrow(X), ncol = ncol(Y))
    # print(pvalue); print(x);print(y)
    allocation = c_set <= cut
    return(allocation)
    # x0 = x[length(x)/2 + 0.5]
    # y0 = x[length(y)/2 + 0.5]

}
two_planes = function(X, Y, Z, beta, iter_max = 10)
{
    c_set = beta[1]*X + beta[2]*Y
    o = order(c_set)
    # X = X[o]; Y = Y[o]; Z = Z[o]; c_set = c_set[o]
    allocation = matrix(FALSE, nrow = length(c_set), ncol = length(c_set))
    for(i in 1:length(c_set))
    {
        allocation[, i] = c_set <= c_set[i]
    }
    # print(allocation)
    # print(12345)
    error = rep(0, length = length(c_set))
    c = sample(c_set, 1)
    # print(c)
    # print('c1')
    indicator = c_set <= c
    # print(indicator)
    while(sum(indicator) <= 7 | sum(indicator) >= length(c_set)-7)
    {
        c = sample(c_set, 1)
        indicator = c_set <= c
    }
    c_old = c
    M = matrix(0, nrow = 3, ncol = 3)
    for(i in 1:iter_max)
    {
        # c = sample(c_set, 1)
        indicator = c_set <= c
        if(sum(indicator) <= 4 | sum(indicator) >= length(c_set)-4)
        {
            # print('Bad')
            return(c(c, -1))
        }
        # print('Good')
        x = X[indicator]; y = Y[indicator]; z = Z[indicator]
        # print(z)
        M[1, 1] = length(x)
        M[1, 2] = M[2, 1] = sum(x)
        M[1, 3] = M[3, 1] = sum(y)
        M[2, 3] = M[3, 2] = sum(x*y)
        M[2, 2] = sum(x * x)
        M[3, 3] = sum(y * y)
        # M = solve(M)
        # print('success 2')
        # print(c(x,y,z))
        z0 = c(sum(z), sum(x*z), sum(y*z))
        # beta1 = (M %*% z0)
        beta1 = qr.coef(qr(M), z0); beta1[is.na(beta1)] = 0
        # print(M)
        # print(z)
        error1 = ((beta1[1] + beta1[2]*X + beta1[3]*Y - Z)^2)
        # print(error1)
        x = X[!indicator]; y = Y[!indicator]; z = Z[!indicator]
        M[1, 1] = length(x)
        M[1, 2] = M[2, 1] = sum(x)
        M[1, 3] = M[3, 1] = sum(y)
        M[2, 3] = M[3, 2] = sum(x*y)
        M[2, 2] = sum(x * x)
        M[3, 3] = sum(y * y)
        # M = solve(M)
        # print(x); print(y); print(z); print(M)
        # print('success 3')
        z0 = c(sum(z), sum(x*z), sum(y*z))
        # beta2 = (M %*% z0)
        beta2 = qr.coef(qr(M), z0); beta2[is.na(beta2)] = 0
        error2 = ((beta2[1] + beta2[2]*X + beta2[3]*Y - Z)^2)
        # print(error2)
        # for(j in 1:length(c_set))
        # {
        #     error[j] = sum(error1*allocation[, j] + error2*(!allocation[, j]))
        # }
        error = colSums(error1*allocation + error2*(!allocation))
        # print(error)
        c_old = c
        p = which.min(error)
        c = c_set[p]
        RSS = error[p]
        if(c_old == c)
        {
            # print('Same c')
            return(c(c, RSS))
        }
        # print(c(c, RSS))
    }
    return(c(c, -1))
}

kernel = function(x, y)
{
    outer(x, y, triangular_2d)
}
npr = function(x0, y0, x, y, z, h = 0.5, ker = kernel, l = 0.04, jump = TRUE)
{
    # print(c(x0, y0))
    x_window = which(abs(x - x0) <= l)
    y_window = which(abs(y - y0) <= l)
    x = x[x_window]; y = y[y_window]; z = z[x_window, y_window]
    if(jump)
    {
        allocation = tryCatch(expr = {seperator(x, y, z)}, error = function(e){print(e);matrix(TRUE, nrow = length(x), ncol = length(y))})
        # allocation = seperator(x, y, z)
        if(allocation[which.min(abs(x - x0)), which.min(abs(y - y0))] == FALSE)
        {
            allocation = !allocation
        }
        wt = ker((x - x0)/h, (y - y0)/h) * allocation
    }
    else
    {
        wt = ker((x - x0)/h, (y - y0)/h)
    }
    return(sum(wt * z)/sum(wt))
}
# print(npr(x[1], y[1], x, y, z))
npr_2d = function(x0, y0, x, y, z, h = 0.5, ker = kernel, l = 0.03, jump = TRUE)
{
    mapply(npr, x0, y0, MoreArgs = list(x=x, y=y, z=z, h = h, ker = ker, l = l + .Machine$double.eps*100, jump = jump))
}


# h = seq(0.10, 1.0, by=0.05)
# mse = NULL
# for(k in 1:length(h))
# {
#     Z = outer(x, y, npr_2d, x=x, y=y, z=z, h=h[k])
#     mse[k] = mean((z-Z)^2) ################### z0 or z
#     print(h[k]); print('Done.')
# }
# plot(h, mse)
# print(h[which.min(mse)])
# abline(v=h[which.min(mse)])

# Z = outer(x, y, npr_2d, x=x, y=y, z=z, h=h[which.min(mse)])
Z = outer(x, y, npr_2d, x=x, y=y, z=z, h=0.9, l = 0.04)

persp3D(z = Z)
print('Done. Click on plot to continue')
locator(1)


# z = z0 + rnorm(nrow(z0)*ncol(z0), 0, 0.5)
# persp3D(z = z)
lpr = function(x0, y0, x, y, z, h = 0.5, ker = kernel, l = 0.04, jump = TRUE)
{
    x_window = which(abs(x - x0) <= l)
    y_window = which(abs(y - y0) <= l)
    x = x[x_window]; y = y[y_window]; z = z[x_window, y_window]
    if(jump)
    {
        allocation = tryCatch(expr = {seperator(x, y, z)}, error = function(e){print(e);matrix(TRUE, nrow = length(x), ncol = length(y))})
        # allocation = seperator(x, y, z)
        if(allocation[which.min(abs(x - x0)), which.min(abs(y - y0))] == FALSE)
        {
            allocation = !allocation
        }
        wt = ker((x - x0)/h, (y - y0)/h) * allocation
    }
    else
    {
        wt = ker((x - x0)/h, (y - y0)/h)
    }
    # wt = ker((x - x0)/h, (y - y0)/h)
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
# print(lpr(x[1], y[1], x, y, z))

lpr_2d = function(x0, y0, x, y, z, h = 0.5, ker = kernel, l = 0.03, jump = TRUE)
{
    mapply(lpr, x0, y0, MoreArgs = list(x=x, y=y, z=z, h = h, ker = ker, l = l + .Machine$double.eps*100, jump = jump))
}
Z = outer(x, y, lpr_2d, x=x, y=y, z=z, h=0.9, l = 0.04)
persp3D(z = Z)

# h = seq(0.05, 0.5, by=0.05)
# mse = NULL
# for(k in 1:length(h))
# {
#     Z = outer(x, y, lpr_2d, x=x, y=y, z=z, h=h[k])
#     mse[k] = mean((z-Z)^2) ############ z0 or z
#     print(h[k]); print('Done.')
# }
# plot(h, mse)
# print(h[which.min(mse)])
# abline(v=h[which.min(mse)])

# Z = outer(x, y, lpr_2d, x=x, y=y, z=z, h=h[which.min(mse)])

# persp3D(z = Z)
print('Complete.. ')
