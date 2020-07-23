library(plot3D)
x = y = 0:99/100
f = function(x, y)
{
    2*x + 3*y^2 + 5.5 + 9.0*x*x*as.numeric(x>0.5)
    # sin((x+y)) + as.numeric(x > 0.5) * cos((x+y)) * 0.2
    # 10 * x^2 #+ 0.5*y^2 * as.numeric(x>0.5)
}
z = outer(x, y, f)
z = z + rnorm(nrow(z) * ncol(z), 0, 1)
# persp3D(z = z)

index = x >= 0.46 & x <= 0.54
x = x[index]
y = y[index]
z = outer(x, y, f)
z = z + rnorm(nrow(z) * ncol(z), 0, 1)
persp3D(z = z)

X = mesh(x, y)$x
Y = mesh(x, y)$y
model = lm(as.vector(z) ~ as.vector(X) + as.vector(Y))$coefficie
Zhat = model[1] + model[2]*X + model[3]*Y
persp3D(z = Zhat, x = X, y=Y)

M = matrix(0, nrow = 3, ncol = 3)
seperator = function(X, Y, Z)
{
    x = as.vector(X); y = as.vector(Y); z = as.vector(Z)
    M[1, 1] = length(x)
    M[1, 2] = M[2, 1] = sum(x)
    M[1, 3] = M[3, 1] = sum(y)
    M[2, 3] = M[3, 2] = sum(x*y)
    M[2, 2] sum(y * y)
    M[3, 3] sum(x * x)
    M = solve(M)
    z = c(sum(z), sum(x*z), sum(y*z))
    beta = (m %*% z)[2:3]
    x0 = x[length(x)/2 + 0.5]
    y0 = x[length(y)/2 + 0.5]

}
two_planes = function(X, Y, Z, beta)
{
    c_set = beta[1]*X + beta[2]*Y
    o = order(c_set)
    X = X[order]; Y = Y[order]; Z = Z[order]
    c = sample(c_set, 1)
    indicator = 
}