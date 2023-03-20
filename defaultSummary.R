defaultSummary
###
#i<-names(train)
#output.names <- i[1:4]
#input.names <- i[-c(1:4)]

#BETFormula <- as.formula(paste(paste(output.names, collapse = '+'), '~',
 #                              paste(input.names, collapse = '+')))#
library(caret)
library(tidyverse)



set.seed(1000)
index<-sample(1:nrow(Dataset),round(0.7*nrow(Dataset)))
train<-Dataset[index,]
test<-Dataset[-index,]

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

#defaultSummary<-read.csv('C:\\Users\\admin\\Documents\\defaultSummary.csv',T)
#defaultSummary<-defaultSummary[,-1]
predict1=as.data.frame(predict(model1,Dataset,na.rm=T))
names(predict1)<-c('Pre_WL','Pre_WP','Pre_EL','Pre_EP')
names(predict1)<-make.names(names(predict1))
predict1<-cbind(predict1,Dataset[,1:4])

defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(pred=predict1$Pre_WL,obs=predict1$WCPO_LLCPUE_BET)),
                            defaultSummary(data.frame(pred=predict1$Pre_WP,obs=predict1$WCPO_PSCPUE_BET)),
                            defaultSummary(data.frame(pred=predict1$Pre_EL,obs=predict1$EPO_LLCPUE_BET)),
                            defaultSummary(data.frame(pred=predict1$Pre_EP,obs=predict1$EPO_PSCPUE_BET))))