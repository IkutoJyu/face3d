color.cyl<-function(colors,radius,shift=c(0,0,0),ratio=0.9,div=1000,smooth=F){
	n<-length(colors)
	base.cyl<-turn3d(x=c(-1,-1,1,1)*ratio*radius,y=c(0,1,1,0)*radius,n=div,smooth=smooth)
	base.cyl$vb<-rotationMatrix(pi/2,0,0,1)%*%base.cyl$vb
	base.x<-range(base.cyl$vb[1,])
	base.y<-range(base.cyl$vb[2,])
	base.z<-range(base.cyl$vb[3,])

	half.len<-base.y[2]
	text.pos.ystart<-base.y[1]
	if(n%%2==0){
		base.cyl$vb[2,]<-base.cyl$vb[2,]-(n-1)*half.len
		text.pos.ystart<-text.pos.ystart-(n-1)*half.len
	}
	else{
		base.cyl$vb[2,]<-base.cyl$vb[2,]-(n+1)*half.len
		text.pos.ystart<-text.pos.ystart-(n+1)*half.len
	}

	gen.cyl=lapply(1:n,function(m){each.cyl=base.cyl;each.cyl$vb[2,]=base.cyl$vb[2,]+(m-1)*2*half.len;each.cyl$vb[1:3,]=each.cyl$vb[1:3,]+shift;return(each.cyl)})
	
	text.pos.y<-sapply(0:n,function(m)return(text.pos.ystart+m*2*half.len))+shift[2]
	text.pos.x<-rep(base.x[2],n+1)+0.05*radius+shift[1]+30
	text.pos.z<-rep(base.z[2],n+1)+0.05*radius+shift[3]
	return(list(cyl=gen.cyl,text.pos=cbind(text.pos.x,text.pos.y,text.pos.z)))	
}
