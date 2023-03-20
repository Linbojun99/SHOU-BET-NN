Figure plot
##ggplot
library(maps)
library(ggspatial)
library(ggplot2)
library(tidyverse)
library(eoffice) 
library(cowplot)
library(ggsci)
worldMap <- fortify(map_data("world2"), region = "subregion")

###
library(metR)
library(ggthemes)
library(grDevices) 
library(RColorBrewer)

display.brewer.all(type='seq')

colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(106)

image(volcano,col=colormap)

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


WCPFC<-data.frame(x=c(210,210,230,230,360,360,210),y=c(50,-5,-5,-50,-50,50,50))

for(y in 1995:2019)
{
  data_Dis[data_Dis$Year==y & data_Dis$Fishing=='LL','Value']<-(data_Dis[data_Dis$Year==y& data_Dis$Fishing=='LL','Value']-min(data_Dis[data_Dis$Year==y& data_Dis$Fishing=='LL','Value']))/(max(data_Dis[data_Dis$Year==y& data_Dis$Fishing=='LL','Value'])-min(data_Dis[data_Dis$Year==y& data_Dis$Fishing=='LL','Value']))
  data_Dis[data_Dis$Year==y & data_Dis$Fishing=='PS','Value']<-(data_Dis[data_Dis$Year==y& data_Dis$Fishing=='PS','Value']-min(data_Dis[data_Dis$Year==y& data_Dis$Fishing=='PS','Value']))/(max(data_Dis[data_Dis$Year==y& data_Dis$Fishing=='PS','Value'])-min(data_Dis[data_Dis$Year==y& data_Dis$Fishing=='PS','Value']))
  
  }


###Distribution
ggplot() + 
  #geom_tile(data=data_Dis,aes(x=Lon,y=Lat,fill=Value),interpolate = T)+
  geom_point(data=data_Dis[data_Dis$Value>0,],aes(x=Lon,y=Lat,size=Value,color=Fishing))+
  
  #geom_contour_fill(data=data_Dis,aes(x=Lon,y=Lat,z=Value),binwidth = 0.1)+
  scale_fill_gradientn(colors=colormap)+

  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey95',color='grey60',size=0.5)+
  coord_sf(xlim = c(116, 284),  ylim = c(-44, 44))+
  #annotation_scale(location = "br",  plot_unit = "km",text_cex = 1)+
  #guides(fill=guide_legend(title=NULL))+
  facet_wrap(~Fishing,nrow=2)+
  theme_bw()+  
  scale_color_d3()+
  #annotate("text", x = 180 , y = 23,label = "WCPFC",colour="Black",size=7,alpha=0.7) + 
  #annotate("text", x = 236 , y = 23,label = "IATTC",colour="Black",size=7,alpha=0.7) + 
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
  #geom_polygon(WCPFC,mapping=aes(x=x,y=y),fill='transparent',color='black',linetype=2)+
  labs(size='AI (the normalized CPUE)')


ggsave('Figure1.png',dpi=600)

topptx(filename = 'Figure1.pptx',devsize = T)



plotnet(bet_net)
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


ggplot(data=weights)+
 geom_bar(aes(x=importance, y=reorder(variable, importance)), stat='identity', color=rgb(0,0,1,0.5), fill=rgb(0,0,1,0.5))+
  labs(x = 'Importance', y=NULL)+
  theme_bw()+
  theme(strip.background = element_rect(fill = "white", colour = "white")) +
  theme(axis.title.x = element_text(size=12,face = 'bold'))+
  theme(axis.title.y = element_text(size=12,face = 'bold'))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(title = element_text(size=12))+
  theme(strip.text = element_text(face = 'bold',size=12))+
  theme(legend.text = element_text(size=10))
