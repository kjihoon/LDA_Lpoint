
pr<-function(data,name){
  tab<-table(data[,1])
  line<-paste(data[,2],data[,3],sep=":")
  line<-tapply(line,data[,1],identity)
  line<-Map(c,tab,line)
  line<-lapply(line,function(x) paste(x,collapse = " "))
  line<-paste(line,collapse = "\n")
  loc<-paste0(name,".dat")
  outfile=file(loc)
  writeLines(line,outfile)
  close(outfile)
}

ldafunction<-function(data,k,alpha,beta){
  itemfreq<-recast(data,data$ID~data$PD_S_NM)
  itemfreq2<-melt(itemfreq,id=names(itemfreq)[1])
  itemfreq2<-itemfreq2[order(itemfreq2[,1]),]
  itemname<-as.character(unique(itemfreq2$variable))
  itemfreq2$variable<-rep(1:length(unique(itemfreq2$variable)),length(unique(itemfreq2[,1])))
  itemfreq2<-itemfreq2[itemfreq2$value!=0,]
  itemname<-data.frame("품목"=itemname)
  write.table(itemname,"itemname.txt")
  pr(itemfreq2,"쇼핑")
  shop<-read.documents("쇼핑.dat")
  item_name<-read.vocab("itemname.txt")
  lda<-lda.collapsed.gibbs.sampler(shop,k,item_name,500,alpha,beta)
  return(lda)
}

#####A01
alpha=1.0
beta=1.0
k=10 #토픽수 
itemname<-sort(unique(as.character(newset2_a01$PD_S_NM)))
w=length(itemname)
lda<-ldafunction(newset2_a01,k,alpha,beta)

###theta
theta<-t(lda$document_sums)# document_sum : 특정 토픽에 존재하는 아이템 구매 빈도(id별)
for (i in 1:n) theta[i,] = theta[i,]/sum(theta[i,])
round(theta[1:10,1:k],2)

###phi
phi<-lda$topics
for(i in 1:k) phi[i,] = phi[i,]/sum(phi[i,])  # 특정 토픽의 각 상품의 빈도/특정 토픽의 총빈도수 
round(phi,2)

p=colSums(lda$topics)/sum(lda$topics)  ## 단일 품목의 support 특정상품빈도/전체빈도

lift = matrix(0,nrow=k,ncol=w)
colnames(lift)<-itemname
p<-p[-1]
phi<-phi[,-1]
for (i in 1:k){
  lift[i,p!=0]=phi[i,p!=0]/p[p!=0]
  if(length(lift[i,p==0])!=0){
    lift[i,p==0]=0
  }
}

topic_name<-c()  ##lift 값 기준 크기순 2개의 아이템
for (i in 1:k){
  sorted=sort(lift[i,],decreasing = T)[1:3]
  topic_name<-append(topic_name,paste(names(sorted),collapse = "-"))
}
library(lattice)

colnames(theta)<-topic_name
theta_new<-t(apply(theta,1,function(x) x/sum(x)))
barchart(theta_new[15,])

##토픽 비율 설명 plot
barchart(phi[1,order(phi[1,],decreasing = T)][1:10])
barchart(phi[2,order(phi[2,],decreasing = T)][1:10])
barchart(phi[3,order(phi[3,],decreasing = T)][1:10])
barchart(phi[4,order(phi[4,],decreasing = T)][1:10])
barchart(phi[5,order(phi[5,],decreasing = T)][1:10])


#####A02
alpha=1.0
beta=1.0
k=10 #토픽수 
itemname<-sort(unique(as.character(newset2_a02$PD_S_NM)))
w=length(itemname)
lda<-ldafunction(newset2_a02,k,alpha,beta)

###theta
theta<-t(lda$document_sums)# document_sum : 특정 토픽에 존재하는 아이템 구매 빈도(id별)
for (i in 1:n) theta[i,] = theta[i,]/sum(theta[i,])
round(theta[1:10,1:k],2)

###phi
phi<-lda$topics
for(i in 1:k) phi[i,] = phi[i,]/sum(phi[i,])  # 특정 토픽의 각 상품의 빈도/특정 토픽의 총빈도수 
round(phi,2)

p=colSums(lda$topics)/sum(lda$topics)  ## 단일 품목의 support 특정상품빈도/전체빈도

lift = matrix(0,nrow=k,ncol=w)
colnames(lift)<-itemname
p<-p[-1]
phi<-phi[,-1]
for (i in 1:k){
  lift[i,p!=0]=phi[i,p!=0]/p[p!=0]
  if(length(lift[i,p==0])!=0){
    lift[i,p==0]=0
  }
}

topic_name<-c()  ##lift 값 기준 크기순 2개의 아이템
for (i in 1:k){
  sorted=sort(lift[i,],decreasing = T)[1:3]
  topic_name<-append(topic_name,paste(names(sorted),collapse = "-"))
}
library(lattice)

colnames(theta)<-topic_name
theta_new<-t(apply(theta,1,function(x) x/sum(x)))
barchart(theta_new[15,])

##토픽 비율 설명 plot
barchart(phi[1,order(phi[1,],decreasing = T)][1:10])
barchart(phi[2,order(phi[2,],decreasing = T)][1:10])
barchart(phi[3,order(phi[3,],decreasing = T)][1:10])
barchart(phi[4,order(phi[4,],decreasing = T)][1:10])
barchart(phi[5,order(phi[5,],decreasing = T)][1:10])


#####A03
alpha=1.0
beta=1.0
k=5 #토픽수 
itemname<-sort(unique(as.character(newset2_a03$PD_S_NM)))
w=length(itemname)
lda<-ldafunction(newset2_a03,k,alpha,beta)

###theta
theta<-t(lda$document_sums)# document_sum : 특정 토픽에 존재하는 아이템 구매 빈도(id별)
for (i in 1:n) theta[i,] = theta[i,]/sum(theta[i,])
round(theta[1:10,1:k],2)

###phi
phi<-lda$topics
for(i in 1:k) phi[i,] = phi[i,]/sum(phi[i,])  # 특정 토픽의 각 상품의 빈도/특정 토픽의 총빈도수 
round(phi,2)

p=colSums(lda$topics)/sum(lda$topics)  ## 단일 품목의 support 특정상품빈도/전체빈도

lift = matrix(0,nrow=k,ncol=w)
colnames(lift)<-itemname
p<-p[-1]
phi<-phi[,-1]
for (i in 1:k){
  lift[i,p!=0]=phi[i,p!=0]/p[p!=0]
  if(length(lift[i,p==0])!=0){
    lift[i,p==0]=0
  }
}

topic_name<-c()  ##lift 값 기준 크기순 2개의 아이템
for (i in 1:k){
  sorted=sort(lift[i,],decreasing = T)[1:3]
  topic_name<-append(topic_name,paste(names(sorted),collapse = "-"))
}
library(lattice)

colnames(theta)<-topic_name
rownames(theta)<-sort(unique(newset2_a03$ID))
theta_new<-t(apply(theta,1,function(x) x/sum(x)))
barchart(theta_new[15,])

##토픽 비율 설명 plot
barchart(phi[1,order(phi[1,],decreasing = T)][1:10])
barchart(phi[2,order(phi[2,],decreasing = T)][1:10])
barchart(phi[3,order(phi[3,],decreasing = T)][1:10])
barchart(phi[4,order(phi[4,],decreasing = T)][1:10])
barchart(phi[5,order(phi[5,],decreasing = T)][1:10])


#####A04
alpha=1.0
beta=1.0
k=10 #토픽수 
itemname<-sort(unique(as.character(newset2_a04$PD_S_NM)))
w=length(itemname)
lda<-ldafunction(newset2_a04,k,alpha,beta)

###theta
theta<-t(lda$document_sums)# document_sum : 특정 토픽에 존재하는 아이템 구매 빈도(id별)
for (i in 1:n) theta[i,] = theta[i,]/sum(theta[i,])
round(theta[1:10,1:k],2)

###phi
phi<-lda$topics
for(i in 1:k) phi[i,] = phi[i,]/sum(phi[i,])  # 특정 토픽의 각 상품의 빈도/특정 토픽의 총빈도수 
round(phi,2)

p=colSums(lda$topics)/sum(lda$topics)  ## 단일 품목의 support 특정상품빈도/전체빈도

lift = matrix(0,nrow=k,ncol=w)
colnames(lift)<-itemname
p<-p[-1]
phi<-phi[,-1]
for (i in 1:k){
  lift[i,p!=0]=phi[i,p!=0]/p[p!=0]
  if(length(lift[i,p==0])!=0){
    lift[i,p==0]=0
  }
}

topic_name<-c()  ##lift 값 기준 크기순 2개의 아이템
for (i in 1:k){
  sorted=sort(lift[i,],decreasing = T)[1:3]
  topic_name<-append(topic_name,paste(names(sorted),collapse = "-"))
}
library(lattice)

colnames(theta)<-topic_name
theta_new<-t(apply(theta,1,function(x) x/sum(x)))
barchart(theta_new[15,])

##토픽 비율 설명 plot
barchart(phi[1,order(phi[1,],decreasing = T)][1:10])
barchart(phi[2,order(phi[2,],decreasing = T)][1:10])
barchart(phi[3,order(phi[3,],decreasing = T)][1:10])
barchart(phi[4,order(phi[4,],decreasing = T)][1:10])
barchart(phi[5,order(phi[5,],decreasing = T)][1:10])


#####A05
alpha=1.0
beta=1.0
k=3 #토픽수 
itemname<-sort(unique(as.character(newset2_a05$PD_S_NM)))
w=length(itemname)
lda<-ldafunction(newset2_a05,k,alpha,beta)

###theta
theta<-t(lda$document_sums)# document_sum : 특정 토픽에 존재하는 아이템 구매 빈도(id별)
for (i in 1:n) theta[i,] = theta[i,]/sum(theta[i,])
round(theta[1:10,1:k],2)

###phi
phi<-lda$topics
for(i in 1:k) phi[i,] = phi[i,]/sum(phi[i,])  # 특정 토픽의 각 상품의 빈도/특정 토픽의 총빈도수 
round(phi,2)

p=colSums(lda$topics)/sum(lda$topics)  ## 단일 품목의 support 특정상품빈도/전체빈도

lift = matrix(0,nrow=k,ncol=w)
colnames(lift)<-itemname
p<-p[-1]
phi<-phi[,-1]
for (i in 1:k){
  lift[i,p!=0]=phi[i,p!=0]/p[p!=0]
  if(length(lift[i,p==0])!=0){
    lift[i,p==0]=0
  }
}

topic_name<-c()  ##lift 값 기준 크기순 2개의 아이템
for (i in 1:k){
  sorted=sort(lift[i,],decreasing = T)[1:3]
  topic_name<-append(topic_name,paste(names(sorted),collapse = "-"))
}
library(lattice)

colnames(theta)<-topic_name
rownames(theta)<-sort(unique(newset2_a05$ID))
theta_new<-t(apply(theta,1,function(x) x/sum(x)))
barchart(theta[15,])

##토픽 비율 설명 plot
barchart(phi[1,order(phi[1,],decreasing = T)][1:10])
barchart(phi[2,order(phi[2,],decreasing = T)][1:10])
barchart(phi[3,order(phi[3,],decreasing = T)][1:10])
barchart(phi[4,order(phi[4,],decreasing = T)][1:10])
barchart(phi[5,order(phi[5,],decreasing = T)][1:10])
