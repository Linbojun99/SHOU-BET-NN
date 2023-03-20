BET_NN
#packages
###packages
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



summary(data)

data<-data[,c(1:8,9,16,37,46,53,60)]
colnames(data)<-c('Year','Month','Lon','Lat',
                  'WCPO_LLCPUE_BET','WCPO_PSCPUE_BET','EPO_LLCPUE_BET','EPO_PSCPUE_BET',
                  'SST','SSS','SSH','O2','Nppv','ONI')
data<-na.omit(data)
data<-subset(data,data$Lon>=120 & data$Lon<=280 & data$Lat>=-40 &data$Lat<=40)

###????Ԥ????
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





###
library(metR)
library(ggthemes)
library(grDevices) 
library(RColorBrewer)
library(ggspatial)
library(ggplot2)
colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(15)

worldMap <- fortify(map_data("world2"), region = "subregion")



set.seed(1000)
index<-sample(1:nrow(Dataset),round(0.7*nrow(Dataset)))
train<-Dataset[index,]
test<-Dataset[-index,]




i<-names(train)
output.names <- i[1:4]
input.names <- i[-c(1:4)]

BETFormula <- as.formula(paste(paste(output.names, collapse = '+'), '~',
                               paste(input.names, collapse = '+')))
bet_net <- neuralnet(BETFormula, train, hidden=11, act.fct="tanh", linear.output=T, 
                     algorithm = "rprop+",lifesign = "full",threshold = 0.6)


plotnet(bet_net)
ggsave('plotnet3.png',dpi=300)

olden(bet_net)
ggsave('olden3.png',dpi=300)

garson(bet_net)




predict_final<-predict(bet_net,newdata=Dataset)

pmaxs<-apply(predict_final,2,max,na.rm=T)
pmins<-apply(predict_final,2,min,na.rm=T)

scaled_predict_final<-as.data.frame(scale(predict_final,center = pmins , scale =pmaxs - pmins),na.rm=T)
colnames(scaled_predict_final)<-c('Pre_WL','Pre_WP','Pre_EL','Pre_EP')
summary(scaled_predict_final)
#scaled_predict_final<-(scaled_predict_final-min(scaled_predict_final))/(max(scaled_predict_final)-min(scaled_predict_final))
newdata<-cbind(data,scaled_predict_final)
#newdata$Pre_WL<-(newdata$Pre_WL-min(newdata$Pre_WL))/(max(newdata$Pre_WL)-min(newdata$Pre_WP))



for(y in 1995:2019)
{
  newdata[newdata$Year==y,'Pre_WL']<-(newdata[newdata$Year==y,'Pre_WL']-min(newdata[newdata$Year==y,'Pre_WL']))/(max(newdata[newdata$Year==y,'Pre_WL'])-min(newdata[newdata$Year==y,'Pre_WL']))
  newdata[newdata$Year==y,'Pre_WP']<-(newdata[newdata$Year==y,'Pre_WP']-min(newdata[newdata$Year==y,'Pre_WP']))/(max(newdata[newdata$Year==y,'Pre_WP'])-min(newdata[newdata$Year==y,'Pre_WP']))
  newdata[newdata$Year==y,'Pre_WL']<-(newdata[newdata$Year==y,'Pre_EL']-min(newdata[newdata$Year==y,'Pre_EL']))/(max(newdata[newdata$Year==y,'Pre_EL'])-min(newdata[newdata$Year==y,'Pre_EL']))
  newdata[newdata$Year==y,'Pre_WL']<-(newdata[newdata$Year==y,'Pre_EP']-min(newdata[newdata$Year==y,'Pre_EP']))/(max(newdata[newdata$Year==y,'Pre_EP'])-min(newdata[newdata$Year==y,'Pre_EP']))
  
}



###newdata 
a=sum((bet_net[["weights"]][[1]][[2]][,1])^2)/(sum((bet_net[["weights"]][[1]][[2]][,1])^2)+sum((bet_net[["weights"]][[1]][[2]][,2])^2)+sum((bet_net[["weights"]][[1]][[2]][,3])^2)+sum((bet_net[["weights"]][[1]][[2]][,4])^2))
b=sum((bet_net[["weights"]][[1]][[2]][,2])^2)/(sum((bet_net[["weights"]][[1]][[2]][,1])^2)+sum((bet_net[["weights"]][[1]][[2]][,2])^2)+sum((bet_net[["weights"]][[1]][[2]][,3])^2)+sum((bet_net[["weights"]][[1]][[2]][,4])^2))
c=sum((bet_net[["weights"]][[1]][[2]][,3])^2)/(sum((bet_net[["weights"]][[1]][[2]][,1])^2)+sum((bet_net[["weights"]][[1]][[2]][,2])^2)+sum((bet_net[["weights"]][[1]][[2]][,3])^2)+sum((bet_net[["weights"]][[1]][[2]][,4])^2))
d=sum((bet_net[["weights"]][[1]][[2]][,4])^2)/(sum((bet_net[["weights"]][[1]][[2]][,1])^2)+sum((bet_net[["weights"]][[1]][[2]][,2])^2)+sum((bet_net[["weights"]][[1]][[2]][,3])^2)+sum((bet_net[["weights"]][[1]][[2]][,4])^2))


newdata$Pre_CPUE=newdata$Pre_WL*a+newdata$Pre_WP*b+newdata$Pre_EL*c+newdata$Pre_EP*d

summary(newdata$Pre_CPUE)

newdata$Pre_CPUE<-(newdata$Pre_CPUE-min(newdata$Pre_CPUE))/(max(newdata$Pre_CPUE)-min(newdata$Pre_CPUE))
