library(plot3D);library(plot3Drgl);library(png)

f = function(x){x = x-(x%/%(2*pi))*2*pi; exp(-x)}
f = function(x){x = x-(x%/%(2*pi))*2*pi; y=rnorm(1,0,2)+rnorm(1,0,2)*sin(x)+rnorm(1,0,2)*cos(x)+rnorm(1,0,2)*sin(2*x)+rnorm(1,0,2)*cos(2*x)+rnorm(1,0,2)*sin(3*x)+rnorm(1,0,2)*cos(3*x); y=abs(y); 0.3+(y+4-abs(y-4))/2}
x=0:100/100; t=2*pi*x
# par(mfrow=c(4,4), mar=c(0,0,0,0))
# for(i in 1:16)
# {
# 	z=f(t); plot(z*cos(t), z*sin(t), ty='l', axes=FALSE, asp=1)
# }
# png('Desktop/temp.png');par(mar=c(0,0,0,0))
# z=f(t); plot(0, xlim=c(-5,5), ylim=c(-5,5), ty='n', axes=FALSE, asp=1, xlab='', ylab='');polygon(z*cos(t),z*sin(t), col=1)
# dev.off()

f1=function(x,y)
{
	-x*x-y*y
}
f2=function(x,y)
{
	sin((x+y)*pi)
}

funset=list(
	function(x,y){x*x+y*y},
	function(x,y){-x*x-y*y},
	function(x,y){sin(pi*(x+y))},
	function(x,y){x*y},
	function(x,y){x+y}
	)

genimg=function(funspace, i=0, shape=f)
{
	x=0:100/100; t=2*pi*x
	png('Desktop/temp.png',height=128,width=128);par(mar=c(0,0,0,0))
	z=shape(t)
	plot(0, xlim=c(-5,5), ylim=c(-5,5), ty='n', axes=FALSE, asp=1, xlab='', ylab='')
	polygon(z*cos(t),z*sin(t), col=1)
	dev.off()
	png(paste0('Desktop/output.',i,'.png'),height=128,width=128);par(mar=c(0,0,0,0))
	plot(z*cos(t),z*sin(t), col=1, xlim=c(-5,5), ylim=c(-5,5), ty='l', axes=FALSE, asp=1, xlab='', ylab='')
	dev.off()
	# plot(0, xlim=c(-5,5), ylim=c(-5,5), ty='n', axes=FALSE, asp=1, xlab='', ylab='')
	# polygon(z*cos(t),z*sin(t), col=1)
	z=readPNG('Desktop/temp.png')[,,1]
	z=!z
	y=x=0:127/127
	n=length(funspace)
	n=sample(n,2)
	# cat(n);cat('\n')
	f1=funspace[[n[1]]];f2=funspace[[n[2]]]
	v=outer(x,y,f1)*runif(1,0,3)
	w=outer(x,y,f2)*runif(1,0,3)
	# v=matrix(0, nrow=length(x),ncol=length(y))
	# w=matrix(1, nrow=length(x),ncol=length(y))
	Z=v
	Z[z!=0]=w[z!=0]
	Z=t(Z[nrow(Z):1,])
	png(paste0('Desktop/input.',i,'.png'),height=128,width=128);par(mar=c(0,0,0,0))
	image2D(Z, colkey=FALSE)
	dev.off()
	Z
}