library(arulse)
#장바구니 생성
basket<-tapply(as.character(newset2$PD_M_NM),as.character(newset2$ID_DT),unique)
basket<-sapply(basket,list)
basket_a01<-tapply(as.character(newset2_a01$PD_M_NM),as.character(newset2_a01$ID_DT),unique)
basket_a01<-sapply(basket_a01,list)
basket_a02<-tapply(as.character(newset2_a02$PD_M_NM),as.character(newset2_a02$ID_DT),unique)
basket_a02<-sapply(basket_a02,list)
basket_a03<-tapply(as.character(newset2_a03$PD_M_NM),as.character(newset2_a03$ID_DT),unique)
basket_a03<-sapply(basket_a03,list)
basket_a04<-tapply(as.character(newset2_a04$PD_M_NM),as.character(newset2_a04$ID_DT),unique)
basket_a04<-sapply(basket_a04,list)
basket_a05<-tapply(as.character(newset2_a05$PD_S_NM),as.character(newset2_a05$ID_DT),unique)
basket_a05<-sapply(basket_a05,list)

#아이템 유사도 군집d
tran_a05<-as(basket,"transactions")
itemFrequencyPlot(tran_a05,topN=20)
itemtable_a05<-crossTable(tran_a05)

a<-apriori(tran_a05, control=list(verbos=T), parameter=list(support=0.0005, confidence=0.6,minlen=2)) 




