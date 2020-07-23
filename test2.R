if(!require(plot3D)){
    install.packages("plot3D")
    library(plot3D)
}
# if(!require(plot3Drgl)){
#     install.packages("plot3Drgl")
#     library(plot3Drgl)
# }
functionset=list(
    function(x,y){x*x+y*y},
    function(x,y){-x*x-y*y},
    function(x,y){sin(pi*(x+y))},
    function(x,y){x*y},
    function(x,y){x+y}
    )

genimg = function(funspace = functionset,seed = NA)
{
    if(!is.na(seed))
    {
        set.seed(seed)
    }
    x = y = 0:9/10
    m = length(x); n = length(y)
    z = matrix(NA, nrow = m, ncol = n)
    startrow = sample(1:m, 1, prob = sqrt(m:1))
    print(startrow)
    endrow = sample(startrow:m, 1, prob = sqrt(1:(m - startrow + 1)))
    print(endrow)
    startcolumn = NULL
    startcolumn[startrow] = sample(1:n, 1)
    z[startrow, 1:startcolumn[startrow]] = 1
    i = startrow + 1
    while(i <= endrow)
    {
        v = max(startcolumn[i-1]-2, 1):min(startcolumn[i-1]+2, n)
        startcolumn[i] = sample(v, 1)
        z[i, 1:startcolumn[i]] = 1
        i = i + 1
    }
    print(startcolumn)
    fn = length(funspace)
    j = sample(1:fn, 2, replace = FALSE)
    f1 = funspace[[j[1]]]
    f2 = funspace[[j[2]]]
    original = z
    original[is.na(original)] = 0
    Z = outer(x, y, f1)
    z[!is.na(z)] = Z[!is.na(z)]
    Z = outer(x, y, f2)
    z[is.na(z)] = Z[is.na(z)]
    list(category = original, z = z)
}
z = genimg()
print(z)
persp3D(z=z$z)
x = rep(0:9/10, length(0:9/10)); y = rep(0:9/10, each = length(0:9/10)); Z = as.vector(z$z); category = as.vector(z$category)
scatter3D(x, y, Z, col = NA)
points3D(x[category > 0], y[category > 0], Z[category > 0], col = 1, add = TRUE)
points3D(x[category == 0], y[category == 0], Z[category == 0], col = 2, add = TRUE)
dev.new()
scatter3D(x, y, Z)
# plotrgl()