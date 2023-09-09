#ICES-Evaluating the impacts of environmental and fishery variability on the distribution of bigeye tuna in the  Pacific Ocean
#neural network R code
#


#packages
#install.packages('MASS')
library('MASS')
#install.packages('nnet')
library('nnet')
#install.packages('neuralnet')
library('neuralnet')
#install.packages('NeuralNetTools')
library('NeuralNetTools')
library(tidyverse)
library(ggplot2)

#data
data<-read.csv('Full_PO_BET_Env.csv',T)
summary(data)

data<-data[,c(1:8,9,16,37,46,53,60)]#data select
colnames(data)<-c('Year','Month','Lon','Lat',
                  'WCPO_LLCPUE_BET','WCPO_PSCPUE_BET','EPO_LLCPUE_BET','EPO_PSCPUE_BET',
                  'SST','SSS','SSH','O2','Nppv','ONI')
data<-na.omit(data)
data<-subset(data,data$Lon>=120 & data$Lon<=280 & data$Lat>=-40 &data$Lat<=40)#pacific ocean


#Transforming the continuous variables

cont<- data[,c('WCPO_LLCPUE_BET','WCPO_PSCPUE_BET','EPO_LLCPUE_BET','EPO_PSCPUE_BET','Month','Lon','Lat','SST','SSS','SSH','O2','Nppv','ONI')]

maxs<-apply(cont,2,max,na.rm=T)
mins<-apply(cont,2,min,na.rm=T)

scaled_cont<-as.data.frame(scale(cont,center = mins , scale =maxs - mins),na.rm=T)



##Combine all data into single modeling data
Dataset<-cbind(scaled_cont)

colnames(Dataset)<-c('WCPO_LLCPUE_BET','WCPO_PSCPUE_BET','EPO_LLCPUE_BET','EPO_PSCPUE_BET',
                     'Month','Lon','Lat','SST','SSS','SSH','O2','Nppv',
                     'ONI')

#
set.seed(1000)
index<-sample(1:nrow(Dataset),round(0.7*nrow(Dataset)))
train<-Dataset[index,]
test<-Dataset[-index,]


#neuralnet model
i<-names(train)
output.names <- i[1:4]
input.names <- i[-c(1:4)]

BETFormula <- as.formula(paste(paste(output.names, collapse = '+'), '~',
                               paste(input.names, collapse = '+')))


#cross validation
MSE=matrix(data=NA,ncol=10,nrow=100)
ARV=matrix(data=NA,ncol=10,nrow=100)

for(i in 1:100)
{
  index = sample(1:nrow(Dataset),0.7*nrow(Dataset),replace = F)
  train = Dataset[index,]
  test = Dataset[-index,]
  
  
  #model1<-  nnet(form, train, size = 4, decay = 0.01, maxit = 10000, linout = T, trace = F,na.rm=T) 
  #model2<-  nnet(form, train, size = 5, decay = 0.01, maxit = 10000, linout = T, trace = F,na.rm=T) 
  #model3<-  nnet(form, train, size = 6, decay = 0.01, maxit = 10000, linout = T, trace = F,na.rm=T) 
  #model4<-  nnet(form, train, size = 7, decay = 0.01, maxit = 10000, linout = T, trace = F,na.rm=T) 
  #model5<-  nnet(form, train, size = 8, decay = 0.01, maxit = 10000, linout = T, trace = F,na.rm=T) 
  #model6<-  nnet(form, train, size = 9, decay = 0.01, maxit = 10000, linout = T, trace = F,na.rm=T) 
  #model7<-  nnet(form, train, size = 10, decay = 0.01, maxit = 10000, linout = T, trace = F,na.rm=T) 
  #model8<-  nnet(form, train, size = 11, decay = 0.01, maxit = 10000, linout = T, trace = F,na.rm=T) 
  #model9<-  nnet(form, train, size = 12, decay = 0.01, maxit = 10000, linout = T, trace = F,na.rm=T) 
  #model10<-  nnet(form, train, size = 13, decay = 0.01, maxit = 10000, linout = T, trace = F,na.rm=T) 
  
 
  model1 <- neuralnet(BETFormula, train, hidden=4, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.6)
  model2 <- neuralnet(BETFormula, train, hidden=5, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.6)
  model3 <- neuralnet(BETFormula, train, hidden=6, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.6)
  model4 <- neuralnet(BETFormula, train, hidden=7, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.6)
  model5 <- neuralnet(BETFormula, train, hidden=8, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.6)
  model6 <- neuralnet(BETFormula, train, hidden=9, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.6)
  model7 <- neuralnet(BETFormula, train, hidden=10, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.6)
  model8 <- neuralnet(BETFormula, train, hidden=11, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.6)
  model9 <- neuralnet(BETFormula, train, hidden=12, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.6)
  model10 <- neuralnet(BETFormula, train, hidden=13, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.6)
  
  
  predict1=predict(model1,test,na.rm=T)
  predict2=predict(model2,test,na.rm=T)
  predict3=predict(model3,test,na.rm=T)
  predict4=predict(model4,test,na.rm=T)
  predict5=predict(model5,test,na.rm=T)
  predict6=predict(model6,test,na.rm=T)
  predict7=predict(model7,test,na.rm=T)
  predict8=predict(model8,test,na.rm=T)
  predict9=predict(model9,test,na.rm=T)
  predict10=predict(model10,test,na.rm=T)
  
  
  
  
  MSE_Model1=sum((predict1-test$BET)^2,na.rm=T)/nrow(test)
  MSE_Model2=sum((predict2-test$BET)^2,na.rm=T)/nrow(test)
  MSE_Model3=sum((predict3-test$BET)^2,na.rm=T)/nrow(test)
  MSE_Model4=sum((predict4-test$BET)^2,na.rm=T)/nrow(test)
  MSE_Model5=sum((predict5-test$BET)^2,na.rm=T)/nrow(test)
  MSE_Model6=sum((predict6-test$BET)^2,na.rm=T)/nrow(test)
  MSE_Model7=sum((predict7-test$BET)^2,na.rm=T)/nrow(test)
  MSE_Model8=sum((predict8-test$BET)^2,na.rm=T)/nrow(test)
  MSE_Model9=sum((predict9-test$BET)^2,na.rm=T)/nrow(test)
  MSE_Model10=sum((predict10-test$BET)^2,na.rm=T)/nrow(test)
  
  
  MSE[i,1]=MSE_Model1
  MSE[i,2]=MSE_Model2
  MSE[i,3]=MSE_Model3
  MSE[i,4]=MSE_Model4
  MSE[i,5]=MSE_Model5
  MSE[i,6]=MSE_Model6  
  MSE[i,7]=MSE_Model7  
  MSE[i,8]=MSE_Model8
  MSE[i,9]=MSE_Model9
  MSE[i,10]=MSE_Model10
  
  
  
  ARV_Model1=sum((predict1-test$BET)^2,na.rm=T)/sum((mean(test$BET)-test$BET)^2,na.rm=T)
  ARV_Model2=sum((predict2-test$BET)^2,na.rm=T)/sum((mean(test$BET)-test$BET)^2,na.rm=T)
  ARV_Model3=sum((predict3-test$BET)^2,na.rm=T)/sum((mean(test$BET)-test$BET)^2,na.rm=T)
  ARV_Model4=sum((predict4-test$BET)^2,na.rm=T)/sum((mean(test$BET)-test$BET)^2,na.rm=T)
  ARV_Model5=sum((predict5-test$BET)^2,na.rm=T)/sum((mean(test$BET)-test$BET)^2,na.rm=T)
  ARV_Model6=sum((predict6-test$BET)^2,na.rm=T)/sum((mean(test$BET)-test$BET)^2,na.rm=T)
  ARV_Model7=sum((predict7-test$BET)^2,na.rm=T)/sum((mean(test$BET)-test$BET)^2,na.rm=T)
  ARV_Model8=sum((predict8-test$BET)^2,na.rm=T)/sum((mean(test$BET)-test$BET)^2,na.rm=T)
  ARV_Model9=sum((predict9-test$BET)^2,na.rm=T)/sum((mean(test$BET)-test$BET)^2,na.rm=T)
  ARV_Model10=sum((predict10-test$BET)^2,na.rm=T)/sum((mean(test$BET)-test$BET)^2,na.rm=T)
  
  
  ARV[i,1]=ARV_Model1
  ARV[i,2]=ARV_Model2
  ARV[i,3]=ARV_Model3
  ARV[i,4]=ARV_Model4
  ARV[i,5]=ARV_Model5
  ARV[i,6]=ARV_Model6  
  ARV[i,7]=ARV_Model7  
  ARV[i,8]=ARV_Model8
  ARV[i,9]=ARV_Model9
  ARV[i,10]=ARV_Model10
  
  
  

}





#model
bet_net <- neuralnet(BETFormula, Dataset, hidden=12,  linear.output=T, 
                     algorithm = "rprop+",lifesign = "full",threshold = 0.01,stepmax=10000)


#prediction
predict_final<-predict(bet_net,newdata=Dataset)

pmaxs<-apply(predict_final,2,max,na.rm=T)
pmins<-apply(predict_final,2,min,na.rm=T)

scaled_predict_final<-as.data.frame(scale(predict_final,center = pmins , scale =pmaxs - pmins),na.rm=T)
colnames(scaled_predict_final)<-c('Pre_WL','Pre_WP','Pre_EL','Pre_EP')
summary(scaled_predict_final)
newdata<-cbind(data,scaled_predict_final)



#net figures
plotnet(bet_net)
ggsave('Figure 3.png',dpi=600)#png / emf



#weights computation
a=sum((bet_net[["weights"]][[1]][[2]][,1])^2)/(sum((bet_net[["weights"]][[1]][[2]][,1])^2)+sum((bet_net[["weights"]][[1]][[2]][,2])^2)+sum((bet_net[["weights"]][[1]][[2]][,3])^2)+sum((bet_net[["weights"]][[1]][[2]][,4])^2))
b=sum((bet_net[["weights"]][[1]][[2]][,2])^2)/(sum((bet_net[["weights"]][[1]][[2]][,1])^2)+sum((bet_net[["weights"]][[1]][[2]][,2])^2)+sum((bet_net[["weights"]][[1]][[2]][,3])^2)+sum((bet_net[["weights"]][[1]][[2]][,4])^2))
c=sum((bet_net[["weights"]][[1]][[2]][,3])^2)/(sum((bet_net[["weights"]][[1]][[2]][,1])^2)+sum((bet_net[["weights"]][[1]][[2]][,2])^2)+sum((bet_net[["weights"]][[1]][[2]][,3])^2)+sum((bet_net[["weights"]][[1]][[2]][,4])^2))
d=sum((bet_net[["weights"]][[1]][[2]][,4])^2)/(sum((bet_net[["weights"]][[1]][[2]][,1])^2)+sum((bet_net[["weights"]][[1]][[2]][,2])^2)+sum((bet_net[["weights"]][[1]][[2]][,3])^2)+sum((bet_net[["weights"]][[1]][[2]][,4])^2))

newdata$Pre_CPUE=newdata$Pre_WL*a+newdata$Pre_WP*b+newdata$Pre_EL*c+newdata$Pre_EP*d
summary(newdata$Pre_CPUE)

newdata$Pre_CPUE<-(newdata$Pre_CPUE-min(newdata$Pre_CPUE))/(max(newdata$Pre_CPUE)-min(newdata$Pre_CPUE))

###
w1=bet_net[["weights"]][[1]][[1]][2,]
w2=bet_net[["weights"]][[1]][[1]][3,]
w3=bet_net[["weights"]][[1]][[1]][4,]
w4=bet_net[["weights"]][[1]][[1]][5,]
w5=bet_net[["weights"]][[1]][[1]][6,]
w6=bet_net[["weights"]][[1]][[1]][7,]
w7=bet_net[["weights"]][[1]][[1]][8,]
w8=bet_net[["weights"]][[1]][[1]][9,]
w9=bet_net[["weights"]][[1]][[1]][10,]

Month<-sum(w1^2)/(sum(w1^2)+sum(w2^2)+sum(w3^2)+sum(w4^2)+
                    sum(w5^2)+sum(w6^2)+sum(w7^2)+sum(w8^2)+sum(w9^2))
Lon<-sum(w2^2)/(sum(w1^2)+sum(w2^2)+sum(w3^2)+sum(w4^2)+
                  sum(w5^2)+sum(w6^2)+sum(w7^2)+sum(w8^2)+sum(w9^2))
Lat<-sum(w3^2)/(sum(w1^2)+sum(w2^2)+sum(w3^2)+sum(w4^2)+
                  sum(w5^2)+sum(w6^2)+sum(w7^2)+sum(w8^2)+sum(w9^2))
SST<-sum(w4^2)/(sum(w1^2)+sum(w2^2)+sum(w3^2)+sum(w4^2)+
                  sum(w5^2)+sum(w6^2)+sum(w7^2)+sum(w8^2)+sum(w9^2))
SSS<-sum(w5^2)/(sum(w1^2)+sum(w2^2)+sum(w3^2)+sum(w4^2)+
                  sum(w5^2)+sum(w6^2)+sum(w7^2)+sum(w8^2)+sum(w9^2))
SSH<-sum(w6^2)/(sum(w1^2)+sum(w2^2)+sum(w3^2)+sum(w4^2)+
                  sum(w5^2)+sum(w6^2)+sum(w7^2)+sum(w8^2)+sum(w9^2))
O2<-sum(w7^2)/(sum(w1^2)+sum(w2^2)+sum(w3^2)+sum(w4^2)+
                 sum(w5^2)+sum(w6^2)+sum(w7^2)+sum(w8^2)+sum(w9^2))
NPP<-sum(w8^2)/(sum(w1^2)+sum(w2^2)+sum(w3^2)+sum(w4^2)+
                  sum(w5^2)+sum(w6^2)+sum(w7^2)+sum(w8^2)+sum(w9^2))
ONI<-sum(w9^2)/(sum(w1^2)+sum(w2^2)+sum(w3^2)+sum(w4^2)+
                  sum(w5^2)+sum(w6^2)+sum(w7^2)+sum(w8^2)+sum(w9^2))


weights<-data.frame(Month,Lon,Lat,SST,SSS,SSH,O2,NPP,ONI)
weights<-gather(weights,variable,importance,Month:ONI)