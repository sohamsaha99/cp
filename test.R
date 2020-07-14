library(plot3D)
x = y = 0:99/100
f = function(x, y)
{
    # 2*x + 3*y^2 + 5.5 + 3.0*x*x*as.numeric(x>0.5)
    # sin((x+y)) + as.numeric(x > 0.5) * cos((x+y)) * 0.2
    10 * x^2 #+ 0.5*y^2 * as.numeric(x>0.5)
}
index = x > 0.45 & x <= 0.55 & y > 0.45 & y <= 0.55
print(sum(index))
# index = seq_len(length(x))
x = x[index]
y = y[index]
print(length(x))
# Z = Z[index]
z = outer(x, y, f)
X = rep(x, length(y))
Y = rep(y, each = length(x))
Z = as.vector(z)
Z = Z + 0.09*rnorm(length(Z))
print(length(Z))
# Z = Z / sd(Z)
persp3D(z = matrix(Z, nrow = length(x)))
model0 = lm(Z ~ X + Y + I(X*X) + I(X*Y) + I(Y*Y))
cluster = as.numeric(X > 0.5)
model = lm(Z ~ X + Y + I(X*X) + I(X*Y) + I(Y*Y) + cluster)
# model1 = lm(Z[X>0.5]~X[X>0.5]+Y[X>0.5])
# model2 = lm(Z[X<=0.5]~X[X<=0.5]+Y[X<=0.5])
RSS0 = sum(model0$res^2)
RSS = sum(model$res^2)
# RSS1 = sum(model1$res^2)
# RSS2 = sum(model2$res^2)
# RSS = RSS1 + RSS2
df1 = 1
df2 = length(Z) - 7
F = ((RSS0-RSS)/df1)/(RSS/df2)
print(RSS0)
print(RSS)
# print(RSS1)
# print(RSS2)
print(F)
pvalue = pf(F, df1, df2, lower.tail = FALSE)
print(c(pvalue, df1, df2))
print(model0)
print(model)
# print(model1)
# print(model2)