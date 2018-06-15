library(stringr)
library(gtools)
library(ggplot2)
library(reshape2)
library(lda)
###data import
set1<-read.table("제4회 Big Data Competition-분석용데이터-01.고객DEMO.txt",sep=",",header=T)
set1$HOM_PST_NO<-na.replace(set1$HOM_PST_NO,999) ## 거주지 na =999
set1<-as.data.frame(lapply(set1,as.factor))
#set2
set2<-read.table("제4회 Big Data Competition-분석용데이터-02.쇼핑업종 상품구매.txt",sep=",",header=T)
#set3
set3<-read.table("제4회 Big Data Competition-분석용데이터-03.쇼핑외 업종 상품구매.txt",sep=",",header=T)
#set4
set4<-readLines("제4회 Big Data Competition-분석용데이터-04.쇼핑업종 상품분류.txt",encoding="UTF-8")
set4<-str_replace_all(set4,",","\t");write(set4,"set4.txt")
set4<-read.table("set4.txt",sep="\t",header=T)
set4<-as.data.frame(lapply(set4,str_replace_all,pattern="\t",replace="/"))


## set2 merge
set2<-cbind(set2,iteminfo=paste0(set2$BIZ_UNIT,"_",set2$PD_S_C))
set4<-cbind(set4,iteminfo=paste0(set4$BIZ_UNIT,"_",set4$PD_S_C))
newset2<-merge(set2,set4[,-c(1:2)],by="iteminfo")
newset2<-cbind(newset2,"ID_DT"=paste0("ID",newset2$ID,"_",newset2$DE_DT))

# 업종별 구분
newset2_a01<-newset2[newset2$BIZ_UNIT=="A01",]
newset2_a02<-newset2[newset2$BIZ_UNIT=="A02",]
newset2_a03<-newset2[newset2$BIZ_UNIT=="A03",]
newset2_a04<-newset2[newset2$BIZ_UNIT=="A04",]
newset2_a05<-newset2[newset2$BIZ_UNIT=="A05",]
