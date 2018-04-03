## R OBJECT

##1번문제
x=c(17,16,20,24,22,15,21,18)

x[x>20] ##1-1

x[x>20]=100  ##1-2
y=x
y

##2번문제
x=matrix(c(rep(-1,25)),5,5) ##2-1
diag(x)=3:7
x

y=x[,-5] #2-2
y

yinfo=c(nrow(y), ncol(y)) ##2-3
yinfo

y2=as.numeric(y) ##2-4
y2[y2==-1]=0
y1=matrix(y2,nrow=5,ncol=4)
y1

##3번문제
write.table(x,"c:\tmp\rowdata.txt",sep=" ", na="NA") ##3-1

is.na(x)#3-2

which((x$v2 != "NA") & (x$v3 != "NA")) #3-3

rdata1=x[which((x$v2 != "NA") & (x$v3 != "NA")),] ##3-4
rdata1

###4번문제
temp=list("TRUE,FALSE", matrix(c(1,0,0,1),2,2), seq(0,1, length.out=100), 4, 5, 6, 7) ##4-1
temp

temp[[2]] <- NULL ##4-2
temp     

temp[[3]] ##4-3

length(temp) ##4-4

##5번문제
a1<-1:2  ##5-1
a2<-1:2
a1+a2
## 2 4 // a1 = c(1,2), a2 = c(1,2)  각 원소들의 합이므로 c(1+1,2+2) = 2 4

a1 <- -(1:2) ##5-2
a2 <- 1:2
a1 + a2
## 0 0 // a1= c(-1, -2), a2 = c(1,2) , 각 원소들의 합 c(-1+1, -2+2) = 0 0

a1<-matrix(0,2,2) ##5-3
a2 <- c(3,4)
a1+a2
## a1은 c(0,0,0,0)인 행렬, a2는 c(3,4)인 벡터이므로 a1+a2=c(0+3,0+4,0+3,0+4)=c(3,4,3,4) 인 행렬이다
     [,1] [,2]
[1,]    3    3
[2,]    4    4

a1<-matrix(1:4,2,2) ##5-4
a1[a1>2]=0
a1
##   [,1] [,2] // a1=c(1,2,3,4)인 행렬이므로 2보다 큰 값을 0으로 바꾼 c(1,2,0,0)인 행렬이다
[1,]    1    0
[2,]    2    0

a1 <- 1:5 ##5-5
a1[-1] - a1[-length(a1)]
## [1] 1 1 1 1 // a1[-1] = a1의 첫번째 원소를 지운 c(2,3,4,5) , 
##              length(a1)= a1의 길이이므로 5, a1[-length(a1)] = a1의 5번째 원소를 지운 c(1,2,3,4),
##              따라서 각 원소들의 차 이므로 c(2-1,3-2,4-3,5-4) = 1 1 1 1


## R PROGRAMMING

##1번문제
a=data.frame(x=1:20, stringsAsFactors = F)
for(i in 1:20)
{
  if(a$x[i]==1)
  {
    a$equal[i]=1
  }else if(a$x[i]==2)
  {
    a$equal[i]=3
  }else{
  a$equal[i]=0.9*a$equal[i-1]-0.1*a$equal[i-2]+1
  }
}
a$equal[20]

##2번문제
a=data.frame(x=1:20, stringsAsFactors = F)
for(i in 1:20)
{
  if(a$x[i]==1)
  {
    a$equal[i]=1
  }else if(a$x[i]==2)
  {
    a$equal[i]=3
  }else{
    a$equal[i]=0.9*a$equal[i-1]-0.1*a$equal[i-2]+1
  if(a$equal[i]>4)
  {
    return(a$x[i])
  }
  }
}
a$x[i]

##3번문제
A <- matrix( runif(100), 50, 5)

for(i in 1:50)
{
  v[i]=sum(A[i,])
}
v

##4번문제
tmp = rep(0, 10)
a <- 10:1
idx = 1
for ( j in a)
{
  if (j<5)
  {
    tmp[idx] <- a[j]
    idx <- idx + 1
  }
}
tmp

> tmp
[1]  7  8  9 10  0  0  0  0  0  0

##5번문제
x=matrix(numeric(),1000 ,5)
for(i in 1:1000)
{
  x[i,]=sample(1:10, 5, replace=F)
}

sid=as.numeric(x)
sid

##6번문제
m.mat=matrix(numeric(),10,5)
for(i in 1:10)
{
  m.mat[i,]=mean(which(sid==i))
}
m.mat

##6번문제

idist=matrix(numeric(),1000,10)
for(i in 1:1000)
{
  for(j in 1:10)
  {
    idist[i,j]=sum(x[i,]*m.mat[j,])/sqrt(sum(x[i,]^2)*sum(m.mat[j,]^2))
  }
}
idist

##7번문제
ivec=numeric()
for(i in 1:1000)
{
  ivec[i]=which(idist[i,]==min(idist[i,]))
}
ivec

##8번문제
set.seed(1)
a=list()
for (i in 1:1000)
{ 
  x=rpois(1,4)+1
  x=min(x,10) 
  a[[i]]=sample(1:10, x)
}

for(i in 1:1000) ##8-1
{
  b[i]=length(a[[i]])
}
table(b)

##8-2
결과값이 맞지않아 구하지 못했습니다

##9번문제
set.seed(1) 
m1 = 10
m2 = 5
num = 0

while(num<4) ##9-1 답 : m1 = 10
{
  if(rbinom(1, 1, 1/2)==0)
  {
    m1=m1-1
    m2=m2+1
    num=num+1
  }else{
    m1=m1+1
    m2=m2-1
    num=num+1
  }
}
m1

while(m1>0 & m2>0) ##9-2 답 : A군이 191번만의 게임에서 이긴다.
{
  if(rbinom(1, 1, 1/2)==0)
  {
    m1=m1-1
    m2=m2+1
    num=num+1
  }else{
    m1=m1+1
    m2=m2-1
    num=num+1
  }
}
m1
m2
num

m=0 ##9-3 A군은 200번의 실험중 129번의 실험에서 이긴다.
for (k in 1:200)
{
  set.seed(k)
  m1 = 10
  m2 = 5
  num = 0

  while(m1>0 & m2>0)
  {
    if(rbinom(1, 1, 1/2)==0)
    {
      m1=m1-1
      m2=m2+1
      num=num+1
    }else{
      m1=m1+1
      m2=m2-1
      num=num+1
    }
  }
  if(m1==15)
  {
    m=m+1
  }
}
m

##10번문제
# 위의 9-3 코드를 이용하여 m2 가 10, 15, 20, 25 일때를 구하면 되므로
# m2=10 일때 99/200, m2=15 일때 78/200, m3=20 일때 67/200, m4=25 일때 60/200 이다.