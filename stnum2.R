##STNUM - TP2

#set working dir
setwd("/Users/Juste/Documents/Cours/STNUM/TP2")

#load data for k=8
paint8=read.csv("painting8.dat",sep=",")

#draw plots
par(bg="cornsilk",lwd=2,col="darkblue",fg="darkblue")
boxplot(paint8)

#visualize matrix of scatterplots
pairs(paint8)
pairs(paint8,fg="darkblue",bg="orange",pch=21,cex=1)

#Proceed to PCA
PCA = prcomp(paint8,retx=T,scale=F)
#get new coordinates
coords = PCA$x
#draw plots
par(mfcol=c(1,2),lwd=2,bg="lightgreen",fg="darkblue",col="darkblue")
boxplot(coords)
plot(coords[,1:2],col="darkred",pch=20,cex=1)

#Separation of two painters
plot(coords[,1:2],type="n")
points(coords[1:40,1:2],col="green",bg="lightblue",pch=20,cex=1)
points(coords[41:83,1:2],col="red",bg="red",pch=20,cex=1)

#same procedre with finer set of data
paint64=read.csv("painting64.dat",sep=",")
PCA64 = prcomp(paint64,retx=T,scale=F)
coords64 = PCA64$x
par(mfcol=c(1,2),lwd=2,bg="lightgreen",fg="darkblue",col="darkblue")
boxplot(coords64)
plot(coords64[,1:2],col="darkred",pch=20,cex=1)
plot(coords64[,1:2],type="n")
points(coords64[1:40,1:2],col="green",bg="lightblue",pch=20,cex=1)
points(coords64[41:83,1:2],col="red",bg="red",pch=20,cex=1)

#Scree graph and correlation circle
layout(matrix(c(1,1,2,3),2,2,byrow=T))
par(bg="lightgreen")
plot(coords64[,1:2],type="n")
points(coords64[1:40,1:2],col="yellow",pch=20,cex=1)
points(coords64[41:83,1:2],col="red",pch=20,cex=1)
text(coords64[1:40,1:2]-c(0.01,0.01),as.character(1:40),font=1,col="yellow")
text(coords64[41:83,1:2]-c(0.01,0.01),as.character(1:40),font=1,col="red")
#screeplot
screeplot(PCA64,xlab="Scree graph")
#correlation circle
library(ade4)
#change names of paint64
colnames(paint64)<-1:63
cormat = cor(paint64,coords64[,1:2])
s.corcircle(cormat)

#calculation of inertia of k-th Principal Components
inertia<-function(k,data){
	if(is.null(data$sdev)){stop("data frame must contain std devs")}
	else{
		dev<-(data$sdev)^2 #variances are \sigma ^2
		if(k>length(dev)){stop("Check dimension!")}
		else{
			return(sum(dev*(c(rep(1,k),rep(0,length(dev)-k))))/sum(dev))
		}
	}
}

#tests
inertia(2,PCA)
inertia(2,PCA64)

i8<-function(k){inertia(k,PCA)}
i64<-function(k){inertia(k,PCA64)}
par(mfcol=c(1,2))
plot(1:63,apply(X=matrix(1:63),MARGIN=1,FUN=i64),ylab="inertia",xlab="Comp number",main="k=64")
plot(1:7,apply(X=matrix(1:7),MARGIN=1,FUN=i8),ylab="inertia",xlab="Comp number",main="k=8")







