#z=matrix(NA, nrow=200, ncol=200)
library(plot3D)
f=function(x, y)
{
	-2*(x-0.5)^2 - 2*(y-0.5)^2 + as.numeric( (x-0.5)^2 + (y-0.5)^2 <= 0.25*0.25)
}
g=function(x,y)
{
	x+y*y
}
# x=y=0:100/100
x=y=0:15/15
z=outer(x, y, f)
z=z+matrix(rnorm(length(z), sd=0.5), nrow=nrow(z))
#persp3D(z=z)
z=cbind(c(NA,x), rbind(y,z))

ker=function(x, y)
{
	#x=x[1];y=y[1]
	#if(x>1|x< -1|y>1|y< -1)
	#	return(0)
	as.numeric(!(x>1|x< -1|y>1|y< -1))*9*(1-x^2)*(1-y^2)/16
}

dataMatrix=function(x,y, degree=2)
{
	#X=mesh(x,y); Y=X$y; X=X$x
	dat=matrix(NA, nrow=length(x)*length(y), ncol=(1+degree)*(1+degree))
	l=0
	for(i in 1:length(y))
	{
		for(j in 1:length(x))
		{
			l=l+1; k=0
			for(k1 in 0:degree)
			{
				for(k2 in 0:degree)
				{
					k=k+1
					dat[l,k]=x[j]^k1*y[i]^k2
				}
			}
		}
	}
	cn=NULL; k=0
	for(k1 in 0:degree)
	{
		for(k2 in 0:degree)
		{
			k=k+1
			cn[k]=paste("x",k1,"y",k2, sep="")
		}
	}
	colnames(dat)=cn
	dat
}

lpr=function(p, z, kernel, hwidth, sd=1 , degree=2)
{
	x=z[-1,1]; y=z[1,-1]; z=z[-1,-1]
	tmp=which.min( abs(x-p[1]) ); tmp[2]=which.min( abs(y-p[2]) )
	if(!(tmp[1]>hwidth))
	{
		if(!(tmp[1]+hwidth<=nrow(z)))
		{
			s=1:nrow(z)
		}else{
			s=1:(2*hwidth+1)
		}
	}else if(!(tmp[1]+hwidth<=nrow(z))){
		s=(tmp[1]-hwidth):nrow(z)
	}else{
		s=(tmp[1]-hwidth):(tmp[1]+hwidth)
	}
	if(!(tmp[2]>hwidth))
	{
		if(!(tmp[2]+hwidth<=ncol(z)))
		{
			t=1:ncol(z)
		}else{
			t=1:(2*hwidth+1)
		}
	}else if(!(tmp[2]+hwidth<=ncol(z))){
		t=(tmp[2]-hwidth):ncol(z)
	}else{
		t=(tmp[2]-hwidth):(tmp[2]+hwidth)
	}
	z=z[s,t]; x=x[s]; y=y[t]
	w=outer((x-p[1])/sd, (y-p[2])/sd, kernel)
	#return(w)	
	w=as.vector(w)
	#X=mesh(x,y); Y=X$y; X=X$x
	dat=list()
	#return(dat)
	dat$xy=dataMatrix(x,y, degree)
	dat$z=as.vector(z)

	k=0; xy=0
	for(k1 in 0:degree)
	{
		for(k2 in 0:degree)
		{
			k=k+1
			xy[k]=p[1]^k1*p[2]^k2
		}
	}
	L=list()
	L$lm=lm(z~xy-1, dat, weights=w)
	L$fit=sum(L$lm$coef*xy)
	L
}

Z=z[-1,-1]
for(i in 1:length(x))
{
	for(j in 1:length(y))
	{
		Z[i,j]=lpr(c(x[i],y[j]), z, ker, hwidth=3, sd=0.3, degree=0)$fit
	}
}

library(rgl)
library(plot3Drgl)
persp3D(mesh(x,y)$x,mesh(x,y)$y,z=Z)
plotrgl()
# scatter3D(z = z)
