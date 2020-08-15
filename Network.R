rm(list= ls());gc()

library(tidyverse)
library(ggplot2)
library(data.table)
library(rpart)
library(rpart.plot)
library(partykit)
library(igraph)
library(rlang)
#--------pre-processing-------------------#
#==set working directory==#
setwd("D:/Contest/SAS")
#==read files=============#
custom1 = fread('customer_1.csv',data.table = F)
custom2 = fread('customer_2.csv',data.table = F)
#300,000 customers
custom =rbind(custom1,custom2)
#200,000 members
member= fread('member.csv',data.table = F)
#--------------------#
#rm(custom1,custom2)
#=========================#

#---------processing----------#
#==not available data=====#
cus_nas=data.frame()
for ( i in 1:ncol(custom)){
  nas=which(is.na(custom[,i])) %>% length()
  col=colnames(custom)[i]
  cus_nas=cus_nas %>% rbind(data.frame(colname = col, Na_s = nas))
}
mem_nas=data.frame()
for ( i in 1:ncol(member)){
  nas=which(is.na(member[,i])) %>% length()
  col=colnames(member)[i]
  mem_nas=mem_nas %>% rbind(data.frame(colname = col, Na_s = nas))
}# all data are available
#--dealing with Nas------#
#===
custom$行銷活動回應次數[which(is.na(custom$行銷活動回應次數))]=0
#===
Code.temp=custom %>% select(郵遞區號,區,縣市)
postalcode = unique(Code.temp[1:nrow(Code.temp),])
custom=custom %>% select(-郵遞區號) %>% left_join(postalcode,by=c("區","縣市"))
#changed nothing due to missing values of district

#-----new variables-----#
custom = custom %>%
  mutate(保險總數= 財富型保障數量+AH保障數量+壽險保障數量+長照型保障數量) %>% 
  mutate(財富型=ifelse(財富型保障數量==0,0,1) %>% as.factor()) %>% 
  mutate(AH型=ifelse(AH保障數量==0,0,1)%>% as.factor()) %>% 
  mutate(壽險型=ifelse(壽險保障數量==0,0,1)%>% as.factor()) %>% 
  mutate(長照型=ifelse(長照型保障數量==0,0,1)%>% as.factor()) %>% 
  mutate(總保單七張以上=ifelse(保險總數>=7,1,0) %>% as.factor())

#---decision tree-------#
set.seed(20191007)

cart.model<- rpart(  總保單七張以上~ 性別+星座+年齡+
                     # 財富型保障數量+
                     # AH保障數量+
                     # 壽險保障數量+
                     # 長照型保障數量+
                     客服進線次數+
                     地址變更次數+
                     服務業務員人數+
                     投訴紀錄+
                     有無更換業務員紀錄+
                     行銷活動回應次數+
                     區+
                     縣市+
                     地區, 
                   data=custom)

# 輸出各節點的細部資訊(呈現在console視窗)
cart.model
prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)  
title('總保單七張以上')

rparty.tree <- as.party(cart.model)
plot(rparty.tree , gp = gpar(fontsize = 6),     # font size changed to 6
     inner_panel=node_inner,
     ip_args=list(
       abbreviate = TRUE, 
       id = FALSE))


#=====custom and member=====#

data=custom %>% left_join(member[,-1], by = "既有客戶編號")
data = data %>% mutate(online=ifelse(is.na(data$一年內網路登入次數),0,1))
data = data %>% mutate(年齡群=年齡%/%10)
data = data %>% mutate(保險類別= paste0(ifelse(財富型保障數量==0,0,1) %>% as.character(),
                                    ifelse(AH保障數量==0,0,1) %>% as.character(),
                                    ifelse(壽險保障數量==0,0,1) %>% as.character(),
                                    ifelse(長照型保障數量==0,0,1) %>% as.character())) %>% 
  filter(!保險類別=='0000')
#---decision tree-------#
set.seed(20191007)

cart.model<- rpart(  長照型~ 性別+星座+年齡+
                      財富型保障數量+
                      AH保障數量+
                      壽險保障數量+
                      #長照型保障數量+
                     客服進線次數+
                     地址變更次數+
                     服務業務員人數+
                     投訴紀錄+
                     有無更換業務員紀錄+
                     行銷活動回應次數+
                    # 區+
                     縣市+
                     地區+
                     一年內旅平險購買次數+
                     一年內網路契約變更次數+
                     一年內網路登入次數, 
                   data=data[which(data$online==1),])

# 輸出各節點的細部資訊(呈現在console視窗)

cart.model
prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)  
title('網路長照型')

rparty.tree <- as.party(cart.model)
plot(rparty.tree , gp = gpar(fontsize = 6),     # font size changed to 6
     inner_panel=node_inner,
     ip_args=list(
       abbreviate = TRUE, 
       id = FALSE))
#=====考慮是否為網路客戶====#
set.seed(20191007)

cart.model<- rpart(  總保單七張以上~ 性別+星座+年齡+
                     # 財富型保障數量+
                     # AH保障數量+
                     # 壽險保障數量+
                     # 長照型保障數量+
                     客服進線次數+
                     地址變更次數+
                     服務業務員人數+
                     投訴紀錄+
                     有無更換業務員紀錄+
                     行銷活動回應次數+
                     # 區+
                     縣市+
                     地區+
                     #一年內旅平險購買次數+
                     #一年內網路契約變更次數+
                     #一年內網路登入次數+
                     online, 
                     data=data)

# 輸出各節點的細部資訊(呈現在console視窗)

cart.model
prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)  
title('混合總保單七張以上型')

rparty.tree <- as.party(cart.model)
plot(rparty.tree , gp = gpar(fontsize = 6),     # font size changed to 6
     inner_panel=node_inner,
     ip_args=list(
       abbreviate = TRUE, 
       id = FALSE))

#-------------Network analysis------------------#
area_list = unique(data %>% filter(nchar(地區)>=1) %>% select(地區)) 
county_list= unique(data %>% filter(nchar(縣市)>=1) %>% select(縣市)) 
district_list = unique(data %>% filter(nchar(區)>=1) %>% select(區))



#--------------地區-------------------#
area.s = area_list[[1]][4]
X1 = '星座'
X2 = '郵遞區號'
target = '壽險保障數量' 


eval(parse(text=paste0("area.data=data %>% filter(地區==area.s)%>%
  filter(nchar(as.character(",X2,"))>=1) %>% 
  filter(nchar(",X1,")>=1)")))  
  

eval(parse(text=paste0("cor_data=area.data %>% group_by(,",X1,",",X2,
                       ") %>% summarise(購買率= sum(",target,")/n())%>% dcast(.,formula =",X2,
                       '~',X1,",value.var= '購買率')")))
  

cor_data[is.na(cor_data)]<-0

corTable = cor(cor_data %>% select(-!!V2))
corTable[corTable<0.03] = 0
network = graph_from_adjacency_matrix(corTable,weighted = T,mode = 'undirected',diag = F)
plot.igraph(network)
title(area.s)
centr_degree(network)$centralization
#--------------縣市-------------------#
county_network=function(county_num,x1,x2,target){
  
county.s = county_list[[1]][county_num]
X1 = x1
X2 = x2
target = target

eval(parse(text=paste0("area.data=data %>% filter(縣市==county.s)%>%
  filter(nchar(as.character(",X2,"))>=1) %>% 
  filter(nchar(",X1,")>=1)")))  

eval(parse(text=paste0("cor_data=area.data %>% group_by(,",X1,",",X2,
                       ") %>% summarise(購買率= sum(",target,")/n())%>% dcast(.,formula =",X2,
                       '~',X1,",value.var= '購買率')")))


cor_data[is.na(cor_data)]<-0
eval(parse(text=paste0("corTable = cor(cor_data %>% select(-",X2,"))")))
corTable[corTable<0.01] = 0
network = graph_from_adjacency_matrix(corTable,weighted = T,mode = 'undirected',diag = F)

plot.igraph(network)
title(county.s)
centr_degree(network)$centralization
}

for(i in 1:nrow(county_list)){
county_network(county_num = i,x1='保險類別',x2='星座',target='保險總數')
}

#------------------------------------#
