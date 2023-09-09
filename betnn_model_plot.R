#betnn_model plot
#packages
library(maps)
library(ggspatial)
library(ggplot2)
library(tidyverse)
library(eoffice) 
library(cowplot)
library(ggsci)
library(metR)
library(ggthemes)
library(grDevices) 
library(RColorBrewer)

worldMap <- fortify(map_data("world2"), region = "subregion")

display.brewer.all(type='seq')

colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(106)



data_Dis<-data[,1:8]
data_Dis<-gather(data_Dis,Variable,Value,WCPO_LLCPUE_BET:EPO_PSCPUE_BET)
data_Dis<-subset(data_Dis,data_Dis$Value>0)

data_Dis$Area=1
data_Dis$Fishing=1

data_Dis[data_Dis$Variable=='WCPO_LLCPUE_BET','Area']<-'WCPO'
data_Dis[data_Dis$Variable=='WCPO_PSCPUE_BET','Area']<-'WCPO'
data_Dis[data_Dis$Variable=='EPO_PSCPUE_BET','Area']<-'EPO'
data_Dis[data_Dis$Variable=='EPO_LLCPUE_BET','Area']<-'EPO'


data_Dis[data_Dis$Variable=='WCPO_LLCPUE_BET','Fishing']<-'LL'
data_Dis[data_Dis$Variable=='EPO_LLCPUE_BET','Fishing']<-'LL'
data_Dis[data_Dis$Variable=='WCPO_PSCPUE_BET','Fishing']<-'PS'
data_Dis[data_Dis$Variable=='EPO_PSCPUE_BET','Fishing']<-'PS'




###Figure 1
{ggplot() + 
  geom_point(data=data_Dis[data_Dis$Value>0,],aes(x=Lon,y=Lat,size=Value,color=Fishing))+
  scale_fill_gradientn(colors=colormap)+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey95',color='grey60',size=0.5)+
  coord_sf(xlim = c(116, 284),  ylim = c(-44, 44))+
  facet_wrap(~Fishing,nrow=2)+
  theme_bw()+  
  scale_color_d3()+
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="serif"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x = element_text(size=14,face = 'bold'))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14))+
  theme(strip.text = element_text(face = 'bold',size=14))+
  theme(legend.text = element_text(size=12))+
  theme(legend.position = 'bottom')+
  scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
  scale_x_longitude(name='Longitude',breaks = seq(120,280,20))+
  labs(size='AI (the normalized CPUE)')

ggsave('Figure1.png',dpi=600)
}


###Figure 2
{
predict1=as.data.frame(predict(model1,Dataset,na.rm=T))
names(predict1)<-c('Pre_WL','Pre_WP','Pre_EL','Pre_EP')
names(predict1)<-make.names(names(predict1))
predict1<-cbind(predict1,Dataset[,1:4])

defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(pred=predict1$Pre_WL,obs=predict1$WCPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict1$Pre_WP,obs=predict1$WCPO_PSCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict1$Pre_EL,obs=predict1$EPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict1$Pre_EP,obs=predict1$EPO_PSCPUE_BET))))


defaultSummary_temp$type=c('WCPO_LL','WCPO_PS','EPO_LL','EPO_PS')
defaultSummary_temp$model='model1'
defaultSummary_temp$iter=i
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)




predict2=as.data.frame(predict(model2,Dataset,na.rm=T))
names(predict2)<-c('Pre_WL','Pre_WP','Pre_EL','Pre_EP')
names(predict2)<-make.names(names(predict2))
predict2<-cbind(predict2,Dataset[,1:4])

defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(pred=predict2$Pre_WL,obs=predict2$WCPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict2$Pre_WP,obs=predict2$WCPO_PSCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict2$Pre_EL,obs=predict2$EPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict2$Pre_EP,obs=predict2$EPO_PSCPUE_BET))))

defaultSummary_temp$type=c('WCPO_LL','WCPO_PS','EPO_LL','EPO_PS')
defaultSummary_temp$model='model2'
defaultSummary_temp$iter=i
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)




predict3=as.data.frame(predict(model3,Dataset,na.rm=T))
names(predict3)<-c('Pre_WL','Pre_WP','Pre_EL','Pre_EP')
names(predict3)<-make.names(names(predict3))
predict3<-cbind(predict3,Dataset[,1:4])

defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(pred=predict3$Pre_WL,obs=predict3$WCPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict3$Pre_WP,obs=predict3$WCPO_PSCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict3$Pre_EL,obs=predict3$EPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict3$Pre_EP,obs=predict3$EPO_PSCPUE_BET))))

defaultSummary_temp$type=c('WCPO_LL','WCPO_PS','EPO_LL','EPO_PS')
defaultSummary_temp$model='model3'
defaultSummary_temp$iter=i
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)



predict4=as.data.frame(predict(model4,Dataset,na.rm=T))
names(predict4)<-c('Pre_WL','Pre_WP','Pre_EL','Pre_EP')
names(predict4)<-make.names(names(predict4))
predict4<-cbind(predict4,Dataset[,1:4])

defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(pred=predict4$Pre_WL,obs=predict4$WCPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict4$Pre_WP,obs=predict4$WCPO_PSCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict4$Pre_EL,obs=predict4$EPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict4$Pre_EP,obs=predict4$EPO_PSCPUE_BET))))

defaultSummary_temp$type=c('WCPO_LL','WCPO_PS','EPO_LL','EPO_PS')
defaultSummary_temp$model='model4'
defaultSummary_temp$iter=i
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)




predict5=as.data.frame(predict(model5,Dataset,na.rm=T))
names(predict5)<-c('Pre_WL','Pre_WP','Pre_EL','Pre_EP')
names(predict5)<-make.names(names(predict5))
predict5<-cbind(predict5,Dataset[,1:4])

defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(pred=predict5$Pre_WL,obs=predict5$WCPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict5$Pre_WP,obs=predict5$WCPO_PSCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict5$Pre_EL,obs=predict5$EPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict5$Pre_EP,obs=predict5$EPO_PSCPUE_BET))))

defaultSummary_temp$type=c('WCPO_LL','WCPO_PS','EPO_LL','EPO_PS')
defaultSummary_temp$model='model5'
defaultSummary_temp$iter=i
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)


predict6=as.data.frame(predict(model6,Dataset,na.rm=T))
names(predict6)<-c('Pre_WL','Pre_WP','Pre_EL','Pre_EP')
names(predict6)<-make.names(names(predict6))
predict6<-cbind(predict6,Dataset[,1:4])

defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(pred=predict6$Pre_WL,obs=predict6$WCPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict6$Pre_WP,obs=predict6$WCPO_PSCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict6$Pre_EL,obs=predict6$EPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict6$Pre_EP,obs=predict6$EPO_PSCPUE_BET))))

defaultSummary_temp$type=c('WCPO_LL','WCPO_PS','EPO_LL','EPO_PS')
defaultSummary_temp$model='model6'
defaultSummary_temp$iter=i
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)



predict7=as.data.frame(predict(model7,Dataset,na.rm=T))
names(predict7)<-c('Pre_WL','Pre_WP','Pre_EL','Pre_EP')
names(predict7)<-make.names(names(predict7))
predict7<-cbind(predict7,Dataset[,1:4])

defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(pred=predict7$Pre_WL,obs=predict7$WCPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict7$Pre_WP,obs=predict7$WCPO_PSCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict7$Pre_EL,obs=predict7$EPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict7$Pre_EP,obs=predict7$EPO_PSCPUE_BET))))

defaultSummary_temp$type=c('WCPO_LL','WCPO_PS','EPO_LL','EPO_PS')
defaultSummary_temp$model='model7'
defaultSummary_temp$iter=i
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)




predict8=as.data.frame(predict(model8,Dataset,na.rm=T))
names(predict8)<-c('Pre_WL','Pre_WP','Pre_EL','Pre_EP')
names(predict8)<-make.names(names(predict8))
predict8<-cbind(predict8,Dataset[,1:4])

defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(pred=predict8$Pre_WL,obs=predict8$WCPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict8$Pre_WP,obs=predict8$WCPO_PSCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict8$Pre_EL,obs=predict8$EPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict8$Pre_EP,obs=predict8$EPO_PSCPUE_BET))))

defaultSummary_temp$type=c('WCPO_LL','WCPO_PS','EPO_LL','EPO_PS')
defaultSummary_temp$model='model8'
defaultSummary_temp$iter=i
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)


predict9=as.data.frame(predict(model9,Dataset,na.rm=T))
names(predict9)<-c('Pre_WL','Pre_WP','Pre_EL','Pre_EP')
names(predict9)<-make.names(names(predict9))
predict9<-cbind(predict9,Dataset[,1:4])

defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(pred=predict9$Pre_WL,obs=predict9$WCPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict9$Pre_WP,obs=predict9$WCPO_PSCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict9$Pre_EL,obs=predict9$EPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict9$Pre_EP,obs=predict9$EPO_PSCPUE_BET))))

defaultSummary_temp$type=c('WCPO_LL','WCPO_PS','EPO_LL','EPO_PS')
defaultSummary_temp$model='model9'
defaultSummary_temp$iter=i
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)


predict10=as.data.frame(predict(model10,Dataset,na.rm=T))
names(predict10)<-c('Pre_WL','Pre_WP','Pre_EL','Pre_EP')
names(predict10)<-make.names(names(predict10))
predict10<-cbind(predict10,Dataset[,1:4])

defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(pred=predict10$Pre_WL,obs=predict10$WCPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict10$Pre_WP,obs=predict10$WCPO_PSCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict10$Pre_EL,obs=predict10$EPO_LLCPUE_BET)),
                                          defaultSummary(data.frame(pred=predict10$Pre_EP,obs=predict10$EPO_PSCPUE_BET))))

defaultSummary_temp$type=c('WCPO_LL','WCPO_PS','EPO_LL','EPO_PS')
defaultSummary_temp$model='model10'
defaultSummary_temp$iter=i
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)



write.csv(defaultSummary,'defaultSummary.csv')



defaultSummary$model[which(defaultSummary$model =='model1')] <- '9-4-4'
defaultSummary$model[which(defaultSummary$model =='model2')] <- '9-5-4'
defaultSummary$model[which(defaultSummary$model =='model3')] <- '9-6-4'
defaultSummary$model[which(defaultSummary$model =='model4')] <- '9-7-4'
defaultSummary$model[which(defaultSummary$model =='model5')] <- '9-8-4'
defaultSummary$model[which(defaultSummary$model =='model6')] <- '9-9-4'
defaultSummary$model[which(defaultSummary$model =='model7')] <- '9-10-4'
defaultSummary$model[which(defaultSummary$model =='model8')] <- '9-11-4'
defaultSummary$model[which(defaultSummary$model =='model9')] <- '9-12-4'
defaultSummary$model[which(defaultSummary$model =='model10')] <- '9-13-4'

defaultSummary$model <- factor(defaultSummary$model,levels=c('9-4-4','9-5-4','9-6-4','9-7-4','9-8-4',
                                                             '9-9-4','9-10-4','9-11-4','9-12-4','9-13-4'))

defaultSummary$type <- factor(defaultSummary$type,levels=c('WCPO_LL','WCPO_PS','EPO_LL','EPO_PS'))
defaultSummary<-defaultSummary[,-2]    
defaultSummary<-gather(defaultSummary,default,value,RMSE:MAE)                                                 

ggplot()+
  geom_boxplot(mapping = aes(x=model,y=value,fill=model),data=defaultSummary)+
  theme_bw()+
  scale_fill_npg()+
  facet_wrap(type~default, scales = "free_y",nrow=4)+
  theme(legend.position = 'none')+
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(text=element_text(family="serif"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.1))+
  theme(axis.title.x =element_text(size=14))+
  theme(axis.title.y = element_text(size=14,face = 'bold'))+
  theme(axis.text.x = element_text(size=12,angle=25,hjust = 0.4, vjust = 0.5))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14,face = 'bold'))+
  theme(strip.text = element_text(face = 'bold',size=12))+
  theme(legend.text = element_text(size=12))

ggsave('Figure 2.png',dpi=600)
}

###Figure 3
{
plotnet(bet_net)
ggsave('Figure 3.png',dpi=600)#png / emf
}

###Figure 4
{
importance1<-olden(bet_net,out_var='WCPO_LLCPUE_BET')

importance2<-olden(bet_net,out_var='WCPO_PSCPUE_BET')

importance3<-olden(bet_net,out_var='EPO_LLCPUE_BET')

importance4<-olden(bet_net,out_var='EPO_PSCPUE_BET')


WL_importance<-(abs(importance1[["data"]][["importance"]]))/sum((abs(importance1[["data"]][["importance"]])))
WL_importance<-data.frame(c(1:9),WL_importance)
colnames(WL_importance)<-c('variable','importance')
WL_importance$variable<-importance1[["data"]][["x_names"]]
WL_importance$output='WCPO_LL'

WP_importance<-(abs(importance2[["data"]][["importance"]]))/sum((abs(importance2[["data"]][["importance"]])))
WP_importance<-data.frame(c(1:9),WP_importance)
colnames(WP_importance)<-c('variable','importance')
WP_importance$variable<-importance2[["data"]][["x_names"]]
WP_importance$output='WCPO_PS'

EL_importance<-(abs(importance3[["data"]][["importance"]]))/sum((abs(importance3[["data"]][["importance"]])))
EL_importance<-data.frame(c(1:9),EL_importance)
colnames(EL_importance)<-c('variable','importance')
EL_importance$variable<-importance3[["data"]][["x_names"]]
EL_importance$output='EPO_LL'

EP_importance<-(abs(importance4[["data"]][["importance"]]))/sum((abs(importance4[["data"]][["importance"]])))
EP_importance<-data.frame(c(1:9),EP_importance)
colnames(EP_importance)<-c('variable','importance')
EP_importance$variable<-importance4[["data"]][["x_names"]]
EP_importance$output='EPO_PS'

importance<-rbind(WL_importance,WP_importance,EL_importance,EP_importance)

WL_importance_plot<-
  ggplot(data=WL_importance)+
  geom_bar(aes(x=importance, y=reorder(variable, importance)), stat='identity', color=rgb(0,0,1,0.5), fill=rgb(0,0,1,0.5))+
  labs(x = 'Importance', y=NULL,subtitle = 'WCPO_LL')+
  theme_bw()+
  theme(strip.background = element_rect(fill = "white", colour = "white")) +
  theme(axis.title.x = element_text(size=12,face = 'bold'))+
  theme(axis.title.y = element_text(size=12,face = 'bold'))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14,face = 'bold'))+
  theme(strip.text = element_text(face = 'bold',size=12))+
  theme(legend.text = element_text(size=12))


WP_importance_plot<-
  ggplot(data=WP_importance)+
  geom_bar(aes(x=importance, y=reorder(variable, importance)), stat='identity', color=rgb(0,0,1,0.5), fill=rgb(0,0,1,0.5))+
  labs(x = 'Importance', y=NULL,subtitle = 'WCPO_PS')+
  theme_bw()+
  theme(strip.background = element_rect(fill = "white", colour = "white")) +
  theme(axis.title.x = element_text(size=12,face = 'bold'))+
  theme(axis.title.y = element_text(size=12,face = 'bold'))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14,face = 'bold'))+
  theme(strip.text = element_text(face = 'bold',size=12))+
  theme(legend.text = element_text(size=12))

EL_importance_plot<-
  ggplot(data=EL_importance)+
  geom_bar(aes(x=importance, y=reorder(variable, importance)), stat='identity', color=rgb(0,0,1,0.5), fill=rgb(0,0,1,0.5))+
  labs(x = 'Importance', y=NULL,subtitle = 'EPO_LL')+
  theme_bw()+
  theme(strip.background = element_rect(fill = "white", colour = "white")) +
  theme(axis.title.x = element_text(size=12,face = 'bold'))+
  theme(axis.title.y = element_text(size=12,face = 'bold'))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14,face = 'bold'))+
  theme(strip.text = element_text(face = 'bold',size=12))+
  theme(legend.text = element_text(size=12))


EP_importance_plot<-
  ggplot(data=EP_importance)+
  geom_bar(aes(x=importance, y=reorder(variable, importance)), stat='identity', color=rgb(0,0,1,0.5), fill=rgb(0,0,1,0.5))+
  labs(x = 'Importance', y=NULL,subtitle = 'EPO_PS')+
  theme_bw()+
  theme(strip.background = element_rect(fill = "white", colour = "white")) +
  theme(axis.title.x = element_text(size=12,face = 'bold'))+
  theme(axis.title.y = element_text(size=12,face = 'bold'))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=14,face = 'bold'))+
  theme(strip.text = element_text(face = 'bold',size=12))+
  theme(legend.text = element_text(size=12))


plot_grid(WL_importance_plot, WP_importance_plot, 
          EL_importance_plot, EP_importance_plot,
          nrow=2 )

ggsave('Figure 4.png',dpi=600)

}

###Figure 5
{
  library(eoffice)
  summary(newdata_Dis[newdata_Dis$Fishing=='LL','Value'])
  newdata_Dis[newdata_Dis$Fishing=='LL','Value']<-(newdata_Dis[newdata_Dis$Fishing=='LL','Value']-min(newdata_Dis[newdata_Dis$Fishing=='LL','Value']))/(max(newdata_Dis[newdata_Dis$Fishing=='LL','Value'])-min(newdata_Dis[newdata_Dis$Fishing=='LL','Value']))
  newdata_Dis[newdata_Dis$Fishing=='PS','Value']<-(newdata_Dis[newdata_Dis$Fishing=='PS','Value']-min(newdata_Dis[newdata_Dis$Fishing=='PS','Value']))/(max(newdata_Dis[newdata_Dis$Fishing=='PS','Value'])-min(newdata_Dis[newdata_Dis$Fishing=='PS','Value']))
  
  ##LL
  ggplot() + 
    geom_contour_fill(data=newdata_Dis[newdata_Dis$Fishing=='LL',],aes(x=Lon,y=Lat,z=Value),binwidth = 0.01)+
    scale_fill_gradientn(colors=colormap)+
    geom_contour(data=newdata_Dis[newdata_Dis$Fishing=='LL',],aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.5,binwidth=0.6,linetype=2)+
    geom_contour(data=newdata_Dis[newdata_Dis$Fishing=='LL',],aes(x=Lon,y=Lat,z=Value),color = "white", size = 0.7,binwidth=0.3,breaks = 0.3)+
    geom_contour(data=newdata_Dis[newdata_Dis$Fishing=='LL',],aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.5,binwidth=0.8)+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey80',color='grey60',size=0.5,alpha=1)+
    coord_sf(xlim = c(120, 280),  ylim = c(-40, 40))+
    theme_bw()+
    theme(strip.background = element_rect(fill = "white", colour = "black")) +
    theme(strip.text = element_text(face = 'bold',size=8))+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
   theme(legend.position = 'none')+
    labs(fill='')+
    scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
    scale_x_longitude(name='Longitude',breaks = seq(120,280,20))
  topptx(filename = 'LL3dlegend.pptx',devsize = T)
  
  
  ##PS
  ggplot() +
    geom_contour_fill(data=newdata_Dis[newdata_Dis$Fishing=='PS',],aes(x=Lon,y=Lat,z=Value),binwidth=0.01)+
    scale_fill_gradientn(colors=colormap)+
    geom_contour(data=newdata_Dis[newdata_Dis$Fishing=='PS',],aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.5,binwidth=0.6,linetype=2)+
    geom_contour(data=newdata_Dis[newdata_Dis$Fishing=='PS',],aes(x=Lon,y=Lat,z=Value),color = "white", size = 0.7,binwidth=0.3,breaks = 0.3)+
    geom_contour(data=newdata_Dis[newdata_Dis$Fishing=='PS',],aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.5,binwidth=0.8)+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey80',color='grey60',size=0.5,alpha=1)+
    coord_sf(xlim = c(120, 280),  ylim = c(-40, 40))+
    theme_bw()+
    theme(strip.background = element_rect(fill = "white", colour = "black")) +
    theme(strip.text = element_text(face = 'bold',size=8))+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    theme(title = element_blank())+
    theme(legend.position = 'none')+
    scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
    scale_x_longitude(name='Longitude',breaks = seq(120,280,20))

  topptx(filename = 'PS3d.pptx',devsize = T)
  
  ###SST
  ggplot() + 
    geom_contour_fill(data=newdata_Dis[newdata_Dis$SST>=17,],aes(x=Lon,y=Lat,z=SST),binwidth=1)+
    scale_fill_gradientn(colors=Redcolormap)+
    geom_contour(data=newdata_Dis[newdata_Dis$SST>=17,],aes(x=Lon,y=Lat,z=SST),color = "black",binwidth=2)+
    geom_text_contour(data=newdata_Dis[newdata_Dis$SST>=17,],aes(x=Lon,y=Lat,z=SST),color = "black",stroke = 0.1,breaks=c(22,24,26,28,30))+
    geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey80',color='grey60',size=0.5,alpha=1)+
    coord_sf(xlim = c(120, 280),  ylim = c(-40, 40))+
    theme_bw()+
    theme(strip.background = element_rect(fill = "white", colour = "black")) +
    theme(strip.text = element_text(face = 'bold',size=8))+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    theme(legend.text = element_text(size=8))+
    theme(legend.position = 'bottom')+
    scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
    scale_x_longitude(name='Longitude',breaks = seq(120,280,20))+
    labs(fill='SST')

  topptx(filename = 'SST3d.pptx',devsize = T)
  topptx(filename = 'SST3dlegend.pptx',devsize = T)
  
  
  ###SSS
  ggplot() + 
    geom_contour_fill(data=newdata_Dis[newdata_Dis$SSS<=37,],aes(x=Lon,y=Lat,z=SSS),binwidth=0.5)+
    scale_fill_gradientn(colors=Purplecolormap)+
    geom_contour(data=newdata_Dis[newdata_Dis$SSS<=37,],aes(x=Lon,y=Lat,z=SSS),color = "black",binwidth=1)+
    geom_text_contour(data=newdata_Dis[newdata_Dis$SSS<=37,],aes(x=Lon,y=Lat,z=SSS),color = "black",breaks=c(34.5,35,35.5,36),stroke = 0.1)+
    geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey80',color='grey60',size=0.5,alpha=1)+
    coord_sf(xlim = c(120, 280),  ylim = c(-40, 40))+
    theme_bw()+
    theme(strip.background = element_rect(fill = "white", colour = "black")) +
    theme(strip.text = element_text(face = 'bold',size=8))+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    theme(legend.text = element_text(size=8))+
    theme(legend.position = 'bottom')+
    scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
    scale_x_longitude(name='Longitude',breaks = seq(120,280,20))+
    labs(fill='SSS')

  topptx(filename = 'SSS3d.pptx',devsize = T)
  topptx(filename = 'SSS3dlegend.pptx',devsize = T)
  
  ###SSH
  ggplot() + 
    geom_contour_fill(data=newdata_Dis,aes(x=Lon,y=Lat,z=SSH),binwidth=0.05)+
    scale_fill_gradientn(colors=Greycolormap)+
    geom_contour(data=newdata_Dis,aes(x=Lon,y=Lat,z=SSH),color = "black",binwidth=0.25)+
    geom_text_contour(data=newdata_Dis,aes(x=Lon,y=Lat,z=SSH),color = "black",stroke = 0.1,breaks=c(-1,-0.75,-0.5,0))+
    geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey80',color='grey60',size=0.5,alpha=1)+
    coord_sf(xlim = c(120, 280),  ylim = c(-40, 40))+
    theme_bw()+
    theme(strip.background = element_rect(fill = "white", colour = "black")) +
    theme(strip.text = element_text(face = 'bold',size=8))+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    theme(legend.text = element_text(size=8))+
    theme(legend.position = 'bottom')+
    scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
    scale_x_longitude(name='Longitude',breaks = seq(120,280,20))+
    labs(fill='SSH')
  topptx(filename = 'SSH3d.pptx',devsize = T)
  topptx(filename = 'SSH3dlegend.pptx',devsize = T)
  
  
  
  ###O2
  ggplot() + 
    geom_contour_fill(data=newdata_Dis[newdata_Dis$O2<260,],aes(x=Lon,y=Lat,z=O2),binwidth=2.5)+
    scale_fill_gradientn(colors=Bluecolormap)+
    geom_contour(data=newdata_Dis[newdata_Dis$O2<260,],aes(x=Lon,y=Lat,z=O2),color = "black",binwidth = 10)+
    geom_text_contour(data=newdata_Dis[newdata_Dis$O2<260,],aes(x=Lon,y=Lat,z=O2),color = "black",breaks=c(180,190,200,210,220,230,240,250),stroke = 0.1)+
    geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey80',color='grey60',size=0.5,alpha=1)+
    coord_sf(xlim = c(120, 280),  ylim = c(-40, 40))+
    theme_bw()+
    theme(strip.background = element_rect(fill = "white", colour = "black")) +
    theme(strip.text = element_text(face = 'bold',size=8))+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    theme(legend.text = element_text(size=8))+
    theme(legend.position = 'bottom')+
    scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
    scale_x_longitude(name='Longitude',breaks = seq(120,280,20))+
    labs(fill='O2')

  topptx(filename = 'O23d.pptx',devsize = T)
  topptx(filename = 'O23dlegend.pptx',devsize = T)
  
  
  
  ###Nppv
  ggplot() + 
    geom_contour_fill(data=newdata_Dis[newdata_Dis$Nppv<15,],aes(x=Lon,y=Lat,z=Nppv),binwidth=1)+
    scale_fill_gradientn(colors=Greencolormap)+
    geom_contour(data=newdata_Dis[newdata_Dis$Nppv<15,],aes(x=Lon,y=Lat,z=Nppv),color = "black",binwidth=5)+
    geom_text_contour(data=newdata_Dis[newdata_Dis$Nppv<15,],aes(x=Lon,y=Lat,z=Nppv),color = "black",stroke = 0.1,breaks=c(1,2,5,6,10),check_overlap=T)+
    geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey80',color='grey60',size=0.5,alpha=1)+
    coord_sf(xlim = c(120, 280),  ylim = c(-40, 40))+
    theme_bw()+
    theme(strip.background = element_rect(fill = "white", colour = "black")) +
    theme(strip.text = element_text(face = 'bold',size=8))+
    theme(axis.title.x = element_text(face = 'bold',size=8))+
    theme(axis.title.y = element_text(face = 'bold',size=8))+
    theme(axis.text.x = element_text(size=8))+
    theme(axis.text.y = element_text(size=8))+

    theme(legend.text = element_text(size=8))+
    theme(legend.position = 'bottom')+
    scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
    scale_x_longitude(name='Longitude',breaks = seq(120,280,20))+
    labs(fill='Nppv')

  topptx(filename = 'Nppv3dlegend.pptx',devsize = T)
  topptx(filename = 'Nppv3d.pptx',devsize = T)
  
}

###Figure 6
{
  newdata_Dis<-newdata[,-c(5:8)]
  newdata_Dis<-gather(newdata_Dis,Variable,Value,Pre_WL:Pre_EP)
  #newdata_Dis<-subset(newdata_Dis,newdata_Dis$Value>0)
  
  newdata_Dis$Area=1
  newdata_Dis$Fishing=1
  
  newdata_Dis[newdata_Dis$Variable=='Pre_WL','Area']<-'WCPO'
  newdata_Dis[newdata_Dis$Variable=='Pre_WP','Area']<-'WCPO'
  newdata_Dis[newdata_Dis$Variable=='Pre_EL','Area']<-'EPO'
  newdata_Dis[newdata_Dis$Variable=='Pre_EP','Area']<-'EPO'
  
  
  newdata_Dis[newdata_Dis$Variable=='Pre_WL','Fishing']<-'LL'
  newdata_Dis[newdata_Dis$Variable=='Pre_WP','Fishing']<-'PS'
  newdata_Dis[newdata_Dis$Variable=='Pre_EL','Fishing']<-'LL'
  newdata_Dis[newdata_Dis$Variable=='Pre_EP','Fishing']<-'PS'
  
  
  
  ggplot(newdata_Dis[newdata_Dis$Fishing == "LL",], aes(x = factor(Year), y = Value)) +
    geom_boxplot() +
    theme_minimal()
  
  
  ggplot(newdata_Dis[newdata_Dis$Fishing == "PS",], aes(x = factor(Year), y = Value)) +
    geom_boxplot() +
    theme_minimal()
  
  
  newdata_Dis9599<-subset(newdata_Dis,newdata_Dis$Year>=1995 & newdata_Dis$Year<=1999)
  newdata_Dis9599p<-ggplot() +
   geom_contour_fill(data=newdata_Dis9599,aes(x=Lon,y=Lat,z=Value),binwidth=0.01)+
    scale_fill_gradientn(colors=colormap)+
    geom_contour(data=newdata_Dis9599,aes(x=Lon,y=Lat,z=Value),color = "white", size = 0.7,binwidth=0.3,breaks = 0.3)+
    geom_contour(data=newdata_Dis9599,aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.5,binwidth=0.6,linetype=2)+
    geom_contour(data=newdata_Dis9599,aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.5,binwidth=0.8)+
   geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey80',color='grey60',size=0.5,alpha=1)+
    coord_sf(xlim = c(120, 280),  ylim = c(-40, 40))+
    theme_bw()+
    facet_grid(Fishing~Year)+
    theme(strip.background = element_rect(fill = "white", colour = "black")) +
    theme(strip.text = element_text(face = 'bold',size=8))+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    theme(title = element_blank())+
    theme(legend.text = element_text(size=8))+
    theme(legend.position = 'none')
  
  newdata_Dis0004<-subset(newdata_Dis,newdata_Dis$Year>=2000 & newdata_Dis$Year<=2004)
  newdata_Dis0004p<-ggplot() + 
 geom_contour_fill(data=newdata_Dis0004,aes(x=Lon,y=Lat,z=Value),binwidth=0.01)+
    scale_fill_gradientn(colors=colormap)+
    geom_contour(data=newdata_Dis0004,aes(x=Lon,y=Lat,z=Value),color = "white", size = 0.7,binwidth=0.3,breaks = 0.3)+
    geom_contour(data=newdata_Dis0004,aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.5,binwidth=0.6,linetype=2)+
    geom_contour(data=newdata_Dis0004,aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.5,binwidth=0.8)+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey80',color='grey60',size=0.5,alpha=1)+
    coord_sf(xlim = c(120, 280),  ylim = c(-40, 40))+
    theme_bw()+
    facet_grid(Fishing~Year)+
    theme(strip.background = element_rect(fill = "white", colour = "black")) +
    theme(strip.text = element_text(face = 'bold',size=8))+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    theme(title = element_blank())+
    theme(legend.text = element_text(size=8))+
    theme(legend.position = 'none')

  
  newdata_Dis0509<-subset(newdata_Dis,newdata_Dis$Year>=2005 & newdata_Dis$Year<=2009)
  newdata_Dis0509p<-ggplot() + 
  geom_contour_fill(data=newdata_Dis0509,aes(x=Lon,y=Lat,z=Value),binwidth=0.01)+
    scale_fill_gradientn(colors=colormap)+
    geom_contour(data=newdata_Dis0509,aes(x=Lon,y=Lat,z=Value),color = "white", size = 0.7,binwidth=0.3,breaks = 0.3)+
    geom_contour(data=newdata_Dis0509,aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.5,binwidth=0.6,linetype=2)+
    geom_contour(data=newdata_Dis0509,aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.5,binwidth=0.8)+
   geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey80',color='grey60',size=0.5,alpha=1)+
    coord_sf(xlim = c(120, 280),  ylim = c(-40, 40))+
    theme_bw()+
    facet_grid(Fishing~Year)+
    theme(strip.background = element_rect(fill = "white", colour = "black")) +
    theme(strip.text = element_text(face = 'bold',size=8))+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    theme(title = element_blank())+
    theme(legend.text = element_text(size=8))+
    theme(legend.position = 'none')

  
  newdata_Dis1014<-subset(newdata_Dis,newdata_Dis$Year>=2010 & newdata_Dis$Year<=2014)
  newdata_Dis1014p<-ggplot() + 
    geom_contour_fill(data=newdata_Dis1014,aes(x=Lon,y=Lat,z=Value),binwidth=0.01)+
    scale_fill_gradientn(colors=colormap)+
    geom_contour(data=newdata_Dis1014,aes(x=Lon,y=Lat,z=Value),color = "white", size = 0.7,binwidth=0.3,breaks = 0.3)+
    geom_contour(data=newdata_Dis1014,aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.5,binwidth=0.6,linetype=2)+
    geom_contour(data=newdata_Dis1014,aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.5,binwidth=0.8)+
    geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey80',color='grey60',size=0.5,alpha=1)+
    coord_sf(xlim = c(120, 280),  ylim = c(-40, 40))+
    theme_bw()+
    facet_grid(Fishing~Year)+
    theme(strip.background = element_rect(fill = "white", colour = "black")) +
    theme(strip.text = element_text(face = 'bold',size=8))+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    theme(title = element_blank())+
    theme(legend.text = element_text(size=8))+
    theme(legend.position = 'none')

  newdata_Dis1519<-subset(newdata_Dis,newdata_Dis$Year>=2015 & newdata_Dis$Year<=2019)
  newdata_Dis1519p<-ggplot() + 
    geom_contour_fill(data=newdata_Dis1519,aes(x=Lon,y=Lat,z=Value),binwidth=0.01)+
    scale_fill_gradientn(colors=colormap)+
    geom_contour(data=newdata_Dis1519,aes(x=Lon,y=Lat,z=Value),color = "white", size = 0.7,binwidth=0.3,breaks = 0.3)+
    geom_contour(data=newdata_Dis1519,aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.5,binwidth=0.6,linetype=2)+
    geom_contour(data=newdata_Dis1519,aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.5,binwidth=0.8)+
    geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey80',color='grey60',size=0.5,alpha=1)+
    coord_sf(xlim = c(120, 280),  ylim = c(-40, 40))+
    theme_bw()+
    facet_grid(Fishing~Year)+
    theme(strip.background = element_rect(fill = "white", colour = "black")) +
    theme(strip.text = element_text(face = 'bold',size=8))+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    theme(legend.text = element_text(size=8))+
    theme(legend.position = 'bottom')+
    labs(fill='AI')

  
  
  plot_grid(newdata_Dis9599p,newdata_Dis0004p,newdata_Dis0509p,
            newdata_Dis1014p,newdata_Dis1519p,nrow=5,
            rel_heights= c(1,1,1,1,1.435))
  
  
  ggsave('Figure6.png',dpi=600)
}

###Figure 7
{
  
  strongla_change1<-subset(newdata_Dis,newdata_Dis$Year==2009  &  newdata_Dis$Month>=10  &  newdata_Dis$Month<=12)
  strongla_change2<-subset(newdata_Dis,newdata_Dis$Year==2011  &  newdata_Dis$Month==1 )
  strongla_change<-rbind(strongla_change1,strongla_change2)
  strongla_change$modes='Strong La Niña'
  moderatela_change<-subset(newdata_Dis,newdata_Dis$Year==2011  &  newdata_Dis$Month>=8  &  newdata_Dis$Month<=12)
  moderatela_change$modes='Moderate-weak La Niña'
  
  neutral_change<-subset(newdata_Dis,newdata_Dis$Year==2013  &  newdata_Dis$Month>=8  &  newdata_Dis$Month<=12)
  neutral_change$modes<-'ENSO-neutral'
  
  moderateel_change<-subset(newdata_Dis,newdata_Dis$Year==2015 &  newdata_Dis$Month>=1  &  newdata_Dis$Month<=5)
  moderateel_change$modes<-'Moderate-weak El Niño'
  
  strongel_change<-subset(newdata_Dis,newdata_Dis$Year==2015 &  newdata_Dis$Month>=8  &  newdata_Dis$Month<=12)
  strongel_change$modes='Strong El Niño'
  
  Distribution_change<-rbind(strongla_change,moderatela_change,neutral_change,
                             moderateel_change,strongel_change)
  
  Distribution_change$modes <- factor(Distribution_change$modes,
                                      levels=c('Strong La Niña',
                                               'Moderate-weak La Niña',
                                               'ENSO-neutral',
                                               'Moderate-weak El Niño',
                                               'Strong El Niño'))
  
  ggplot() + 
    geom_contour_fill(data=Distribution_change,aes(x=Lon,y=Lat,z=Value),binwidth=0.015)+
    scale_fill_gradientn(colors=colormap)+
    geom_contour(data=Distribution_change,aes(x=Lon,y=Lat,z=Value),color = "white", size = 0.7,binwidth=0.3,breaks = 0.3)+
    geom_contour(data=Distribution_change,aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.7,binwidth=0.6,linetype=2)+
    geom_contour(data=Distribution_change,aes(x=Lon,y=Lat,z=Value),color = "black", size = 0.5,binwidth=0.7)+
   geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey80',color='grey60',size=0.5,alpha=1)+
    coord_sf(xlim = c(120, 280),  ylim = c(-40,40))+
    theme_bw()+
    facet_grid(modes~Fishing)+
    theme(strip.background = element_rect(fill = "white", colour = "black")) +
    theme(strip.text = element_text(face = 'bold',size=10))+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    theme(legend.text = element_text(size=8))+
    theme(legend.position = 'bottom')+
    labs(fill='AI')+
    scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
    scale_x_longitude(name='Longitude',breaks = seq(120,280,20))

  ggsave('Figure7.png',dpi=600)
}

###Figure 8
{
  ggplot() + 
    geom_contour_fill(data=newdata,aes(x=Lon,y=Lat,z=Pre_CPUE),binwidth=0.01)+
    scale_fill_gradientn(colors=colormap)+
    geom_contour(data=newdata,aes(x=Lon,y=Lat,z=Pre_CPUE),color = "white", size = 0.5,binwidth=0.3)+
    geom_contour(data=newdata,aes(x=Lon,y=Lat,z=Pre_CPUE),color = "black", size = 0.5,binwidth=0.6,linetype=2)+
    geom_contour(data=newdata,aes(x=Lon,y=Lat,z=Pre_CPUE),color = "black", size = 0.5,binwidth=0.8)+
    geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey80',color='grey60',size=0.5,alpha=1)+
    coord_sf(xlim = c(120, 280),  ylim = c(-40, 40))+
    theme_bw()+
    theme(strip.background = element_rect(fill = "white", colour = "black")) +
    facet_wrap(.~Year)+
    theme(strip.text = element_text(face = 'bold',size=12))+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    theme(legend.text = element_text(size=8))+
    labs(fill='AI')+
    theme(legend.position = 'bottom')+
    scale_y_latitude(name='Latitude',breaks = seq(-40,40,20))+
    scale_x_longitude(name='Longitude',breaks = seq(120,280,20))
  
  ggsave('Figure 8.png',dpi=600)
  
}

###Figure 9
{
  #compute CPUE lon CG
  MG_CPUE_long<-MG_CPUE_long %>%
    mutate(Type= factor(Type,
                        levels = c(),
                        labels = c()))
  
  p1=
    ggplot()+
    geom_path(data=MG_CPUE_long,mapping=aes(x=Value,y=Time,color=Type),size=2)+
    scale_color_d3()+
    scale_y_continuous(trans = 'reverse',breaks = seq(1,300,12))+
    labs(x='Lon',y='Time')+
    theme_minimal()+
    theme(panel.grid.minor = element_blank())+
    facet_wrap(~Type,nrow=1,scales = "free_x")+
    scale_x_longitude(name='Longitude',breaks = seq(140,280,20),limits=c(120,270))
  MG_CPUE_lon$Time<-seq(1,300,1)
  
  p2= 
    ggplot()+
    geom_path(data=MG_CPUE_lon,mapping=aes(x=ONI,y=Time),size=2,color='#3C5488FF')+
    scale_y_continuous(trans = 'reverse',breaks = seq(1,300,12))+
    scale_x_continuous(limits = c(-3,3))+
    theme_minimal()+
    labs(x='ONI index',y='')+
    theme(panel.grid.minor = element_blank())+
    geom_vline(aes(xintercept=0.5),linetype=5,col="red")+
    geom_vline(aes(xintercept=-0.5),linetype=5,col="red")+
    geom_vline(aes(xintercept=1.4),linetype=1,col="red")+
    geom_vline(aes(xintercept=-1.4),linetype=1,col="red")+
    theme(axis.text.y = element_blank())
  

  plot_grid(p1, p2,rel_widths = c(10, 1.5))
  ggsave('Figure 9.png',dpi=600)
  
}

