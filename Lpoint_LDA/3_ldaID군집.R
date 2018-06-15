

tmx<-apply(theta,1,max)

t1mx<-rownames(theta[theta[,1]==tmx,])
t2mx<-rownames(theta[theta[,2]==tmx,])
t3mx<-rownames(theta[theta[,3]==tmx,])
t4mx<-rownames(theta[theta[,4]==tmx,])
t5mx<-rownames(theta[theta[,5]==tmx,])



t1mxset_a05<-newset2_a03[newset2_a03$ID %in% c(t1mx,t3mx,t2mx),]
t1basket_a05<-tapply(as.character(t1mxset_a05$PD_S_NM),as.character(t1mxset_a05$ID_DT),unique)
t1basket_a05<-sapply(t1basket_a05,list)

tran_t1_a05<-as(t1basket_a05,"transactions")
rule_t1_a05<-apriori(tran_t1_a05 ,control=list(verbos=T), parameter=list(support=0.0005,minlen=1)) 

itemFrequencyPlot(tran_t1_a05,topN=20)
itemtable_a05<-crossTable(tran_a05)


basket_a03<-tapply(as.character(newset2_a03$PD_M_NM),as.character(newset2_a03$ID_DT),unique)
basket_a03<-sapply(basket_a03,list)

tran_t1_a05<-as(t1basket_a05,"transactions")
rule_t1_a05<-apriori(tran_t1_a05 ,control=list(verbos=T), parameter=list(support=0.001)) 