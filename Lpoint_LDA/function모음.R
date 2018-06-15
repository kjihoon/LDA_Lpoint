
# 모델 생성을 위한 전처리
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


## LDA 모델 생성
ldafunction<-function(data,k){
  itemfreq<-recast(data,data$ID~data$PD_S_NM)
  itemfreq2<-melt(itemfreq,id=names(itemfreq)[1])
  itemfreq2<-itemfreq2[order(itemfreq2[,1]),]
  itemname<-as.character(unique(itemfreq2$variable))
  w<-length(itemname)
  alpha<-1/k
  beta<-1/w
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



# LDA모델 분석 테이블_리스트
ldaresult_list<-function(lda,itemname){
  resultlist<-list()  
  
  theta<-t(lda$document_sums)# document_sum : 특정 토픽에 존재하는 아이템 구매 빈도(id별) , id vs topic
  for (i in 1:nrow(theta)) theta[i,] = theta[i,]/sum(theta[i,])
  k<-ncol(theta)
  
  phi<-lda$topics   # item vs topic
  for(i in 1:k) phi[i,] = phi[i,]/sum(phi[i,])  
  
  p=colSums(lda$topics)/sum(lda$topics)  ## 단일 품목의 support 특정상품빈도/전체빈도
  w<-ncol(phi)-1
  p<-p[-1]
  phi<-phi[,-1]
  lift = matrix(0,nrow=k,ncol=w)
  
  colnames(lift)<-itemname
  
  for (i in 1:k){
    lift[i,p!=0]=phi[i,p!=0]/p[p!=0]
    if(length(lift[i,p==0])!=0){
      lift[i,p==0]=0
    }
  }
  
  
  #topic_name<-c()  ##lift 값 기준 크기순 2개의 아이템
  #for (i in 1:k){
  #  sorted=sort(lift[i,],decreasing = T)[1:3]
  #  topic_name<-append(topic_name,paste(names(sorted),collapse = "-"))
  #}
  #colnames(theta)<-topic_name
  
  colnames(theta)<-paste0("Topic",c(1:k))
  resultlist[[1]]<-theta
  resultlist[[2]]<-phi
  resultlist[[3]]<-p
  resultlist[[4]]<-lift
  names(resultlist)<-c("theta","phi","p","lift")
  return(resultlist)
}

