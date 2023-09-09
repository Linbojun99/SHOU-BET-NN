#supplement plot
library(dplyr)
library(lubridate)
data_Dis_time<-aggregate(list(Value=data_Dis$Value),
                         by=list(Year=data_Dis$Year,Month=data_Dis$Month,
                                 Variable=data_Dis$Variable,Area=data_Dis$Area,Fishing=data_Dis$Fishing),sum)

data_Dis_time <- data_Dis_time %>%
  #  group_by(Fishing) %>%
  mutate(normalized_value = (Value - min(Value, na.rm = TRUE)) / (max(Value, na.rm = TRUE) - min(Value, na.rm = TRUE)))


data_Dis_time <- data_Dis_time %>%
  mutate(date = make_date(Year, Month)) 

data_Dis_time <- data_Dis_time %>%
  mutate(Variable = ifelse(Variable == "WCPO_LLCPUE_BET", "WCPO_LL", Variable))%>%
  mutate(Variable = ifelse(Variable == "WCPO_PSCPUE_BET", "WCPO_PS", Variable))%>%
  mutate(Variable = ifelse(Variable == "EPO_LLCPUE_BET", "EPO_LL", Variable))%>%
  mutate(Variable = ifelse(Variable == "EPO_PSCPUE_BET", "EPO_PS", Variable))

data_Dis_time$Variable<-factor(data_Dis_time$Variable,levels = c("WCPO_LL", "WCPO_PS", "EPO_LL", "EPO_PS"))

ggplot() +
  geom_line(data=data_Dis_time,mapping= aes(x = date, y = normalized_value,color=Variable),linewidth=1.2) +
  facet_wrap(~Variable,ncol=1)+
  scale_color_npg()+
  theme_bw()+ 
  theme(strip.background = element_rect(fill = "white", colour = "black")) +
  theme(strip.text = element_text(face = 'bold',size=8))+
  labs(x = "Year", y = "Normalized Value(AI)", title = "") +
  theme(legend.position = "none")+
  scale_x_date(limits = as.Date(c("1995-01-01", "2019-12-31")),date_breaks = "1 year", date_labels = "%Y",expand = c(0, 0))

ggsave('Figure S1.emf',dpi=600)



###
#
library(randomForest)
library(ggplot2)
library(gridExtra)

get_importance_ratio <- function(model, model_name) {
  imp <- importance(model)
  total_imp <- sum(imp[,1])
  imp_ratio <- data.frame(Feature = rownames(imp), Importance = imp[,1] / total_imp, Model = model_name)
  return(imp_ratio)
}

imp_model1 <- get_importance_ratio(rf_model1, "WCPO_LL")
imp_model2 <- get_importance_ratio(rf_model2, "WCPO_PS")
imp_model3 <- get_importance_ratio(rf_model3, "EPO_LL")
imp_model4 <- get_importance_ratio(rf_model4, "EPO_PS")


combined_data <- bind_rows(imp_model1, imp_model2, imp_model3, imp_model4)




#
plot_importance <- function(data, model_name) {
  plot <- ggplot(data, aes(x = reorder(Feature, -Importance), y = Importance, fill = Feature)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    xlab('Variables') +
    ylab('Importance') +
    theme_bw() +
    scale_fill_npg()+
    ggtitle(model_name)+
    theme(legend.position = "none")
  return(plot)
}
plot1 <- plot_importance(imp_model1, "WCPO_LL")
plot2 <- plot_importance(imp_model2, "WCPO_PS")
plot3 <- plot_importance(imp_model3, "EPO_LL")
plot4 <- plot_importance(imp_model4, "EPO_PS")


grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

combined_plot <- gridExtra::arrangeGrob(plot1, plot2, plot3, plot4, ncol = 2)

ggsave("Figure S2.png", plot = combined_plot,dpi=600)
