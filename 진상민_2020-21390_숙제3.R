library(alr4)
data(landrent)
new<-landrent[c(1:34),]
X<-new[,c(1:3)]
X<-cbind(matrix(1,nrow=34),X)
Y<-new[,c(5:5)]
X.svd<-svd(X)  # X의 SVD분해
bhat<-X.svd$v%*%solve(diag(X.svd$d))%*%t(X.svd$u)%*%Y
X<-cbind(as.numeric(X[,1]),as.numeric(X[,2]),as.numeric(X[,3]),as.numeric(X[,4]))
s<-t(Y-X%*%bhat)%*%(Y-X%*%bhat)/30
bhat
s
# 7.55(a)

SXX<-X[,c(2:4)]-matrix(1,nrow=34,ncol=34)%*%X[,c(2:4)]/34
SXX<-t(SXX)%*%SXX/33
SYY<-t(X[,c(2:4)])%*%(diag(34)-matrix(1,nrow=34,ncol=34)/34)%*%Y/33
bhat1<-solve(SXX)%*%SYY
bhat0<-mean(Y)-t(bhat1)%*%colMeans(X[,c(2:4)])
bhat1
bhat0
# 7.55(b)
R2<-(t(bhat)%*%t(X)%*%Y-34*mean(Y)^2)/(t(Y)%*%Y-34*mean(Y)^2)
R2a<-(33*R2-3)/30
R2
R2a
# 7.55(c)


F<- R2/3/((1-R2)/30)
F
# 8.38(a)

invxtx<-X.svd$v%*%(solve(diag(X.svd$d)))^2%*%t(X.svd$v)
# SVD 분해를 이용한 역행렬 계산

t1<-(bhat[2])/(sqrt(s*invxtx[2,2]))
t2<-(bhat[3])/(sqrt(s*invxtx[3,3]))
t3<-(bhat[4])/(sqrt(s*invxtx[4,4]))
t1
t2
t3
qt(0.05/2, df=30)
qt(0.05/6, df=30)
# 8.38(b)

x0<-matrix(c(1,15,30,0.5),nrow=1)
x0%*%bhat+qt(0.05/2, df=30)*sqrt(s*(x0%*%invxtx%*%t(x0)))
x0%*%bhat-qt(0.05/2, df=30)*sqrt(s*(x0%*%invxtx%*%t(x0)))
# 8.38(c)

x0%*%bhat+qt(0.05/2, df=30)*sqrt(s*(1+x0%*%invxtx%*%t(x0)))
x0%*%bhat-qt(0.05/2, df=30)*sqrt(s*(1+x0%*%invxtx%*%t(x0)))
# 8.38(d)

