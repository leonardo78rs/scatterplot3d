#install.packages("scatterplot3d")
#install.packages("plotrix")
library(scatterplot3d)
z1 <- c(seq(10, -10, -0.01))
z2 <- c(seq(10, -10, -0.01))
x1 <- cos(z1)
x2 <- cos(-z2)
y1 <- sin(z1)
y2 <- sin(-z2)

z<-c(z1,z2)
x<-c(x1,x2)
y<-c(y1,y2)
scatterplot3d(x1, y1, z1, highlight.3d = TRUE, col.axis = "blue",
              col.grid = "lightblue", main = "Helix", pch = 20)


# http://www.fractarte.com.br/wolframalpha/3d-equations.php


## 1/sin(x2 + y2)
## (Stonehange)


x=0
y=0
z=0
multi=pi
x<-multi*c(seq(-1,1,0.01))
y<-x
z<-x
xx<-c()
yy<-c()
#z<-matrix(,nrow=length(x), ncol = length(y))



for(i in 1:length(x) ) {
    xx<-c(xx,x)   
    yy<-c(yy,replicate(length(y), x[i]))
    
}

z <-  (sin( xx^2 + yy^2) )/2
#z <-  cos(xx*xx+yy*yy)/(xx*xx+yy*yy)
z<-cos(xx*yy)
z<-sin(xx*yy)
z<-log(xx*xx) + log(yy*yy)
#z <- (1/sin(xx)) * (1/sin(yy))
z <-  (sin( xx ) + cos(yy) )
z<- ((xx^3)*yy ) - ( xx * (yy^3))
z <-  (sin( xx )^2 + cos(yy)^2 )
#z<- xx^4+yy^4
z <- exp(-xx*xx-yy*yy)
z <- exp(xx*yy-yy*yy)
#z<- 1 /   (sin( (xx^2) + (yy^2) ) )
z<- cosh(xx) + sinh(yy)

#z<- xx*1

scatterplot3d(xx, yy, z, highlight.3d = TRUE, col.axis = "blue",
              col.grid = "lightblue", main = "Titulo", pch = 46,
              angle=45)
z<--z
scatterplot3d(z, yy,xx , highlight.3d = TRUE, col.axis = "blue",
              col.grid = "lightblue", main = "Titulo", pch = 46,
              angle=125)

