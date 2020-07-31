library(plot3D)
x = y = 0:99/100
f = function(x, y)
{
    2*x + 3*y^2 + 5.5 + 17.0*x*as.numeric(x>0.52)
    # sin((x+y)) + as.numeric(x > 0.5) * cos((x+y)) * 0.2
    # 10 * x^2 #+ 0.5*y^2 * as.numeric(x>0.5)
}
z = outer(x, y, f)
z = z + rnorm(nrow(z) * ncol(z), 0, 100)
# persp3D(z = z)

index = x >= 0.46 & x <= 0.54
x = x[index]
y = y[index]
z = outer(x, y, f)
z = z + rnorm(nrow(z) * ncol(z), 0, 1)
persp3D(z = z)
print('Waiting...')
Sys.sleep(3)
X = mesh(x, y)$x
Y = mesh(x, y)$y
model = lm(as.vector(z) ~ as.vector(X) + as.vector(Y))$coefficie
Zhat = model[1] + model[2]*X + model[3]*Y
persp3D(z = Zhat, x = X, y=Y)

# M = matrix(0, nrow = 3, ncol = 3)
seperator = function(X, Y, Z)
{
    x = as.vector(X); y = as.vector(Y); z = as.vector(Z)
    M = matrix(0, nrow = 3, ncol = 3)
    M[1, 1] = length(x)
    M[1, 2] = M[2, 1] = sum(x)
    M[1, 3] = M[3, 1] = sum(y)
    M[2, 3] = M[3, 2] = sum(x*y)
    M[2, 2] = sum(y * y)
    M[3, 3] = sum(x * x)
    M = solve(M)
    z0 = c(sum(z), sum(x*z), sum(y*z))
    beta = (M %*% z0)[2:3]
    cut = two_planes(x, y, z, beta)
    print(cut)
    c_set = beta[1]*x + beta[2]*y
    allocation = matrix(c_set<=cut, nrow = nrow(X), ncol = ncol(Y))
    return(allocation)
    # x0 = x[length(x)/2 + 0.5]
    # y0 = x[length(y)/2 + 0.5]

}
two_planes = function(X, Y, Z, beta, iter_max = 10)
{
    c_set = beta[1]*X + beta[2]*Y
    o = order(c_set)
    X = X[o]; Y = Y[o]; Z = Z[o]; c_set = c_set[o]
    allocation = matrix(FALSE, nrow = length(c_set), ncol = length(c_set))
    for(i in 1:length(c_set))
    {
        allocation[, i] = c_set <= c_set[i]
    }
    # print(allocation)
    print(12345)
    error = rep(0, length = length(c_set))
    c = sample(c_set, 1)
    print(c)
    print('c1')
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
        if(sum(indicator) <= 7 | sum(indicator) >= length(c_set)-7)
        {
            print('Bad')
            return(c)
        }
        print('Good')
        x = X[indicator]; y = Y[indicator]; z = Z[indicator]
        # print(z)
        M[1, 1] = length(x)
        M[1, 2] = M[2, 1] = sum(x)
        M[1, 3] = M[3, 1] = sum(y)
        M[2, 3] = M[3, 2] = sum(x*y)
        M[2, 2] = sum(y * y)
        M[3, 3] = sum(x * x)
        M = solve(M)
        # print(c(x,y,z))
        z0 = c(sum(z), sum(x*z), sum(y*z))
        beta1 = (M %*% z0)
        # print(M)
        # print(z)
        error1 = ((beta1[1] + beta1[2]*X + beta1[3]*Y - Z)^2)
        # print(error1)
        x = X[!indicator]; y = Y[!indicator]; z = Z[!indicator]
        M[1, 1] = length(x)
        M[1, 2] = M[2, 1] = sum(x)
        M[1, 3] = M[3, 1] = sum(y)
        M[2, 3] = M[3, 2] = sum(x*y)
        M[2, 2] = sum(y * y)
        M[3, 3] = sum(x * x)
        M = solve(M)
        z0 = c(sum(z), sum(x*z), sum(y*z))
        beta2 = (M %*% z0)
        error2 = ((beta2[1] + beta2[2]*X + beta2[3]*Y - Z)^2)
        # print(error2)
        # for(j in 1:length(c_set))
        # {
        #     error[j] = sum(error1*allocation[, j] + error2*(!allocation[, j]))
        # }
        error = colSums(error1*allocation + error2*(!allocation))
        # print(error)
        c_old = c
        c = c_set[which.min(error)]
        if(c_old == c)
        {
            print('Same c')
            return(c)
        }
        print(c)
    }
    return(c)
}


test = function(X ,Y,z, model)
{
    c_set = model[2]*X+model[3]*Y
    c_set  = as.vector(c_set)
    X = as.vector(X)                                                                                                                          
    Y = as.vector(Y)                                                                                                                           
    z = as.vector(z) 
    error = NULL
    for(i in 1:length(c_set))
    {
        index = c_set <= c_set[i]
        x1 = X[index]
        y1 = Y[index]
        z1 = z[index]
        x2 = X[!index]
        y2 = Y[!index]
        z2 = z[!index]
        if(sum(index) == 0 | sum(!index) == 0)
        {
            error[i] = max(error)
        }
        else
        {
            error[i] = sum((z1 - predict(lm(z1~x1+y1)))^2) + sum((z2 - predict(lm(z2~x2+y2)))^2)
        }
    }
    o = order(c_set)
    plot(c_set[o], error[o], ty='b')
    c_set[which.min(error)]
}