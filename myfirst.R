# case study to predict  the intrest rate 
ld_train=read.csv("loan_data_train.csv",stringsAsFactors = F)

ld_test= read.csv("loan_data_test.csv",stringsAsFactors = F)

ld_test$Interest.Rate=NA
ld_train$data='train'
ld_test$data='test'
ld_all=rbind(ld_train,ld_test)

# drop amount funded by investor

ld_all$Amount.Funded.By.Investors=NULL
library(dplyr)
glimpse(ld_all)
lapply(ld_all,function(x) length(unique(x)))
names(ld_all)[sapply(ld_all, function(x) is.character(x))]
 ld_all=ld_all %>%select(-ID)
 # for creating dummy function
 CreateDummies=function(data,var,freq_cutoff=0){
   t=table(data[,var])
   t=t[t>freq_cutoff]
   t=sort(t)
   categories=names(t)[-1]
   
   for( cat in categories){
     name=paste(var,cat,sep="_")
     name=gsub(" ","",name)
     name=gsub("-","_",name)
     name=gsub("\\?","Q",name)
     name=gsub("<","LT_",name)
     name=gsub("\\+","",name)
     name=gsub("\\/","_",name)
     name=gsub(">","GT_",name)
     name=gsub("=","EQ_",name)
     name=gsub(",","",name)
     data[,name]=as.numeric(data[,var]==cat)
   }
   
   data[,var]=NULL
   return(data)
 }
 ld_all$Open.CREDIT.Lines=as.numeric(ld_all$Open.CREDIT.Lines)
 ld_all$Revolving.CREDIT.Balance=as.numeric(ld_all$Revolving.CREDIT.Balance)
 table(ld_all$Loan.Length)
ld_all$Loan.Length=as.numeric(ld_all$Loan.Length=="36 months") 
ld_all$Amount.Requested=as.numeric(ld_all$Amount.Requested)
table(ld_all$Loan.Purpose)
glimpse(ld_all)
library(tidyr)
ld_all=ld_all %>% separate(FICO.Range,into = c("f1","f2"),sep = "-") %>% 
  mutate(f1=as.numeric(f1),
         f2=as.numeric(f2),
         fico=0.5*(f1+f2)) 
ld_all= ld_all %>%select(-f1)
ld_all= ld_all %>%select(-f2)
ld_all= ld_all %>%mutate(Interest.Rate=as.numeric(gsub("%"," ",Interest.Rate)))
ld_all= ld_all %>%mutate(Debt.To.Income.Ratio=as.numeric(gsub("%"," ",Debt.To.Income.Ratio)))
table(ld_all$Loan.Purpose)
ld_all=CreateDummies(ld_all,"Employment.Length",100)
ld_all=CreateDummies(ld_all,"Loan.Purpose",100)
ld_all=CreateDummies(ld_all,"State",100)
ld_all=CreateDummies(ld_all,"Home.Ownership",100)
# removal of null values except  intrest rate and data
for(col in names(ld_all)){
  
  if(sum(is.na(ld_all[,col]))>0 & !(col %in% c("data","Interest.Rate"))){
    
    ld_all[is.na(ld_all[,col]),col]=mean(ld_all[,col],na.rm=T)
  }
  
}
#now seprate train and test data
ld_train=ld_all %>% filter(data=='train') %>% select(-data)
ld_test=ld_all %>% filter(data=='test') %>% select(-data,-Interest.Rate)
# now check the response on training data 
s=sample(1:nrow(ld_train),0.8*nrow(ld_train))
train=ld_train[s,]
test=ld_train[-s,]
fit=lm(Interest.Rate~.,data=train)
library(car)
sort(vif(fit),decreasing = T)[5:1]
fit=step(fit)
summary(fit)
train.score=predict(fit,newdata=test)
train.score
errors=test$Interest.Rate-train.score

errors**2 %>% mean() %>% sqrt()
# now predict on entire data set
fit.final=lm(Interest.Rate~.,data=ld_train)
test.score=predict(fit.final,newdata=ld_test)
View(test.score)
write.csv(test.score,"myfirst.csv")
