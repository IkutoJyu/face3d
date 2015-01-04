require(rgl)
source('color_cylinder.r')

get.coord<-function(string.coord,if3=F){
	z<-unlist(strsplit(string.coord," "))[-1]
	if(if3){
		z<-unlist(strsplit(z,"/"))
	}
	return(as.numeric(z))
}

read.obj<-function(obj.file){
	obj.lines<-readLines(obj.file)
	obj.cv<-obj.lines[grep("^v .*",obj.lines)]
	obj.cvn<-obj.lines[grep("^v .*",obj.lines)]
	obj.cvt<-obj.lines[grep("^vt .*",obj.lines)]
	obj.cf<-obj.lines[grep("^f .*",obj.lines)]
	obj.v<-do.call(cbind,lapply(obj.cv,get.coord))
	obj.vn<-do.call(rbind,lapply(obj.cvn,get.coord))
	obj.vt<-do.call(rbind,lapply(obj.cvt,get.coord))
	obj.fm<-do.call(cbind,lapply(obj.cf,get.coord,if3=T))
	obj.f<-obj.fm[c(1,4,7),]
	obj.ft<-obj.fm[c(2,5,8),]
	obj.fn<-obj.fm[c(3,6,9),]
	return(list(v=obj.v,vn=obj.vn,vt=obj.vt,f=obj.f,fn=obj.fn,ft=obj.ft))
}

create.obj3d<-function(obj,normals=F,texcoord=F){
	mesh.obj<-NULL
	if(!normals&&!texcoord){
		mesh.obj<-tmesh3d(vertices=obj[['v']],indices=obj[['f']],homogeneous=F)
	}
	else{
		if(normals&&!texcoord){
			mesh.obj<-tmesh3d(vertices=obj[['v']],indices=obj[['f']],homogeneous=F,normals=obj[['vn']])
		}
		else{
			if(!normals&&texcoord){
				mesh.obj<-tmesh3d(vertices=obj[['v']],indices=obj[['f']],homogeneous=F,texcoords=obj[['vt']])
			}
			else{
				mesh.obj<-tmesh3d(vertices=obj[['v']],indices=obj[['f']],homogeneous=F,normals=obj[['vn']],texcoords=obj[['vt']])
			}
		}
	}
	return(mesh.obj)
}

ref.obj<-function(ref.obj.file){
	return(read.obj(ref.obj.file))
}

colMesh3d<-function(tmesh3d,color.weight,color.base,color.base.num=length(color.base)){
	col.vtx<-color.base[cut(color.weight,color.base.num)]
	col<-col.vtx[tmesh3d$it]
	return(col)
}

get.loading<-function(loading.face,sd.file,color.base=colorRampPalette(c('blue','grey','red'))(30),color.base.num=length(color.base),feature=1,ncomp=1,comps=1:ncomp){
	loadings<-read.table(loading.face)
	
	list.loading<-lapply(comps,function(m){load.n<-loadings[,m];load.n<-load.n/sqrt(sum(load.n*load.n));return(matrix(load.n,ncol=3))})
	
	sd<-unlist(read.table(sd.file)[feature,comps])
	comb.loadings<-lapply(comps,function(m)return(sd[m]*list.loading[[m]]))
	if(length(comb.loadings)==1){
		comb.loading<-comb.loadings[[1]]
		vtx.loading<-apply(list.loading[[1]],1,function(m)sqrt(sum(m*m)))
	}
	else{
		comb.loading<-Reduce('+',comb.loadings)
		vtx.loading<-apply(comb.loading,1,function(m)sqrt(sum(m*m)))
	}
	col.vtx<-color.base[cut(vtx.loading,color.base.num)]
	col.int<-get.intv(vtx.loading,color.base.num,prec=6)
	return(list(loading=comb.loading,col.vtx=col.vtx,int=col.int))
}

get.intv<-function(vec,num,prec=5){
	cut.vec<-levels(cut(vec,num))
	boundary<-gsub('\\(([^,]+),([^]]+)\\]','\\1',cut.vec)
	return(c(round(as.numeric(boundary),prec),max(vec)))
}	

get.mean.face<-function(ref,mean.face.file){
	mean.face.vtx<-read.table(mean.face.file)
	ref3d<-create.obj3d(ref,F,T)
	ref3d$vb[1:3,]<-t(mean.face.vtx)
	return(ref3d)
}

face.animate<-function(ref.file,mean.face.file,loading.file,sd.file,color.base=terrain.colors(10),feature=1,ncomp=1){
	ref<-ref.obj(ref.file)
	meanface<-get.mean.face(ref,mean.face.file)
	loading.list<-get.loading(loading.file,sd.file,color.base,feature=feature,ncomp=ncomp)
	loading<-t(loading.list[[1]])
	col<-loading.list[[2]][meanface$it]
	int<-loading.list[[3]]
	return(list(meanFace=meanface,loading=loading,col=col,int=int))
}

#if(is.null(meanface)){
#	face.info<-face.animate(ref.file,mean.face.file,loading.file,sd.file,feature=1,ncomp=2)
#	meanface<-face.info$meanFace
#	meanface.vtx<-meanface$vb[1:3,]
#	loading<-face.info$loading
#	col<-face.info$col
#	texture
#}
#rgl.open()
#you shold manually adjust the window size otherwise the out put window will be very small

animate<-function(range,main,texture=NULL,outdir,gifname,number=10){
	outdir<-gsub('\\/$','/',outdir)
	scale<-seq(range[1],range[2],length.out=number)
	#rgl.open()
	rgl.bringtotop()
	rgl.bg(color='white')
	par3d(zoom=0.8)
	sid<-NULL
	plot3d(0,0,0,type='n',xlab='',ylab='',zlab='',xlim=c(-200,200),ylim=c(-200,200),zlim=c(-200,200),main=main)
	#rgl.viewpoint(phi=90)
	#title3d(main="Hello")
	for(s in 1:length(scale)){
		save<-par3d(skipRedraw=TRUE)
		if(!is.null(sid))rgl.pop("shape",sid)
		meanface$vb[1:3,]<<-meanface.vtx+scale[s]*loading
		if(!is.null(texture)){
			sid<-shade3d(meanface,axes=F,col='white',xlab='',ylab='',zlab='',texture=texture,specular='#333333')
		}
		else{
			sid<-shade3d(meanface,axes=F,col=col,xlab='',ylab='',zlab='')
		}
		filename<-paste0(outdir,'pic',formatC(s,digits=1,flag='0'),'.png')
		rgl.snapshot(filename)
		par3d(save)
		Sys.sleep(0.01)
		meanface$vb[1:3,]<<-meanface.vtx
	}
	b<-getwd()
	setwd(outdir)
	system(paste("convert -delay 10 *.png -loop 0",gifname))
	rgl.clear()
	rgl.close()
	setwd(b)
}	
