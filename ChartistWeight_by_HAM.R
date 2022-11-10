rm(list=ls());gc()

library(data.table)
library(ggplot2)
library(doSNOW)
library(foreach)
library(tidyverse)
library(tibbletime)
library(rlang)
#-----Settings---------#
Ft=0
a=1
b=0.05
c=0.02
d=0.95

eta=0.1
lambda=0.45
v_alpha=0.0025
v_beta=0.025
v_gamma=0.0025

N0=100
K0=50
#------read table----------#
setwd("WORKDIR")
TW_pIPOp<-fread("FILE.txt",data.table = F)


#-------------------------#
colnames(TW_pIPOp)<-c("Code","Name","Date","Turnover","Price")
TW_names<-unique(TW_pIPOp$Name)
TW_pIPOp<-TW_pIPOp %>%
  mutate(log_P=log(Price)) %>%
  mutate(Demand_F=c*(Ft-log_P)+rnorm(n=nrow(TW_pIPOp),sd=v_gamma)) %>% 
  mutate(Demand_C=b*(log_P-lag(log_P))+rnorm(n=nrow(TW_pIPOp),sd=v_beta)) %>% 
  mutate(Attr_F=0,Attr_C=0)
#------building model-----#
TW_pIPOp_append<- foreach(ix = 1:length(TW_names),.combine="rbind")%do%{
 test<-TW_pIPOp %>% filter(Name==TW_names[ix])

for(iy in 4:nrow(test)){
  test$Attr_C[iy]=(exp(test$log_P[iy])-exp(test$log_P[iy-1]))*test$Demand_C[iy-2]+d*test$Attr_C[iy-1]
  test$Attr_F[iy]=(exp(test$log_P[iy])-exp(test$log_P[iy-1]))*test$Demand_F[iy-2]+d*test$Attr_F[iy-1]
}
 return(test)
}

TW_pIPOp_append<-TW_pIPOp_append %>% mutate(FtoC=ifelse(Attr_C>Attr_F,0.5+lambda,0.5-lambda),CtoF=ifelse(Attr_F>Attr_C,0.5+lambda,0.5-lambda))




chartist<- foreach(ix = 1:length(TW_names),.combine="rbind")%do%{
  test<-TW_pIPOp_append %>% filter(Name==TW_names[ix])
  test=test %>%
    mutate(N=N0) %>% 
    mutate(K=K0) %>%
    mutate(Deter=runif(nrow(test),min = 0,max=1)) 
  for(iy in 2:nrow(test)){
    test$K[iy]=ifelse(test$Deter[iy]<(N0-test$K[iy-1])/N0*(eta+test$FtoC[iy-1]*(test$K[iy-1])/(N0-1)),test$K[iy-1]+1,ifelse(test$Deter[iy]>(1-(test$K[iy-1]/N0*(eta+test$CtoF[iy-1]*(N0-test$K[iy-1])/(N0-1)))),test$K[iy-1]-1,test$K[iy-1]))
    
  }
  return(test)
}
C_W=chartist %>% mutate(chartist_weight=K/N0)

write.csv(C_W,"ChartistWeight.csv")






