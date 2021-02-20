#Stella Berzina viz
#20.02.21
#data from random websites


library(tidyverse)
library(readxl)
library(extrafont)


df<-read_excel("Deepest_lakes.xlsx")

topten<-df[1:10,]%>%
      mutate(capital=toupper(topten$lake)) #make names capital

fig1<-ggplot(topten, aes(x=reorder(capital, -depth), y=depth, fill=depth))+
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_gradientn(colors=c("#084c61","#489fb5"))+
      coord_polar(theta = "y") +
      
      geom_text(hjust = 0, size = 3, color = "black",aes(y = -2000, label = paste("  ", capital)), family="Bahnschrift") +
      scale_y_continuous(breaks=c(0,-50,-300, -500, -828,-979, -1470, -1637))+
      expand_limits(y=c(0, -2000))+ #makes it bigget than the datapoints (so you have a gap)
      labs(
           title = "WORLD'S DEEPEST LAKES",
           subtitle = "depth in meters | viz by Stella Berzina"
     )+
      theme_bw()+
      theme(
         axis.title.x=element_blank(),
         axis.text.x = element_text(colour ="black", size=11),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         
         
         plot.background = element_rect(fill = "#faf0ca", colour = "NA"),
         plot.title = element_text(size=20, family="Bahnschrift"),
         plot.subtitle = element_text(size=10, family="Bahnschrift"),
         panel.background = element_rect(fill = "#faf0ca", colour = "NA"),
         panel.border =element_blank(),
         
         panel.grid.major = element_line(color="#489fb5", linetype="dotted"), 
         panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         
            
         legend.position = "none",
         plot.margin=unit(c(1,1,1,1),"cm"),
      )+
   annotate("text", x ="LAKE BAIKAL" , y = -85, label = "Most light has\ndisappeared", 
           colour = "#084c61", size=3.5, family="Bahnschrift", vjust=-0.6, hjust=1)+
   annotate("text", x ="LAKE BAIKAL" , y = -350, label = "Height of the\nEiffel Tower   ", 
            colour = "#084c61", size=3.5, family="Bahnschrift", vjust=-0.3, hjust=1)+
   annotate("text", x ="LAKE BAIKAL" , y = -835, label = "Height of the world's\ntallest building", 
            colour = "#084c61", size=3.5, family="Bahnschrift", vjust=0.3, hjust=1.4)+
   annotate("text", x ="LAKE BAIKAL" , y = -1016, label = "Height of the\ntallest waterfall", 
            colour = "#084c61", size=3.5, family="Bahnschrift", vjust=1.7, hjust=0)+
   annotate("text", x ="TANGANYIKA" , y = -1750, 
            label = "Lake Baikal alone holds\n25% of all freshwater\ncontained in world's lakes ", 
            colour = "#084c61", size=3.5, family="Bahnschrift", vjust=-0.2, hjust=0)
fig1

ggsave("deep2.jpg",
       width = 18,
       height = 19.5,
       units = c("cm")
)

