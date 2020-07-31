# X; Y; z; c_set
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
plot(c_set[o], error[o], ty='l')
c_set[which.min(error)]

x = y = 0:99/100
z = 2 + 7*y
index = 0:41
z[-index] = 2 + 2*x[-index] + 6*y[-index]
plot(z)
Z = z+rnorm(100, 0, 0.1)
plot(Z)
z[-index] = 2.3 + 2*x[-index] + 6*y[-index]
Z = z+rnorm(100, 0, 0.1)

c_set = x
error = NULL
for(i in 1:length(c_set))
{
    index = c_set <= c_set[i]
    x1 = x[index]
    z1 = Z[index]
    x2 = x[!index]
    z2 = Z[!index]
    if(sum(index) == 0 | sum(!index) == 0)
    {
        error[i] = max(error)
    }
    else
    {
        error[i] = sum((z1 - predict(lm(z1~x1)))^2) + sum((z2 - predict(lm(z2~x2)))^2)
    }
}
plot(error)
c_set[which.min(error)]
