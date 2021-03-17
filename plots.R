#Juan Buena
#Week 5 Homework
#Kobe Bryant Data


# --- NOTE: Once you load tidyverse, you don't need to load ggplot2
library(tidyverse)



#library(ggplot2)

# --- NOTE: read_csv is faster and uses sensible defaults
data <- read_csv(file="data.csv")
kobe_data <- data[complete.cases(data$shot_made_flag),] #Removed rows with missing data

#Amount of Kobe's shots by range
shot_frequency <- 
  ggplot(kobe_data, aes(x=(shot_zone_range))) +
  geom_bar(fill="darkorchid4") + 
  labs(title="Amount of shots by range", y="Amount", x="Range") +
  theme(legend.title = element_blank(), legend.position = "none")
shot_frequency

#Amount of Kobe's shots by zone (basic)
shot_zone <- 
  ggplot(kobe_data, aes(x=(shot_zone_basic))) +
  geom_bar(fill="goldenrod2") + 
  labs(title="Amount of shots by zone", y="Amount", x="Zone") +
  theme(legend.title = element_blank(), legend.position = "none")
shot_zone

#Kobe's accuracy by zone (basic)
shot_zone_accuracy <- kobe_data %>% group_by(shot_zone_basic) %>%
  summarize(Accuracy = mean(shot_made_flag)*100) %>%
  ggplot(aes(x=shot_zone_basic, y=Accuracy, group=1)) + 
  geom_line(aes(color=Accuracy))+
  geom_point(aes(y=Accuracy, color=Accuracy), size=2)+
  scale_color_gradient(low="gold", high="purple") + 
  labs(title="Kobe's accuracy by zone (basic)", x="Zone", y="Accuracy in %")
shot_zone_accuracy

#Kobe's Accuracy by range
shot_range_accuracy <- kobe_data %>% group_by(shot_zone_range) %>%
  summarize(Accuracy = mean(shot_made_flag)*100) %>%
  ggplot(aes(x=shot_zone_range, y=Accuracy, group=1)) + 
  geom_line(aes(color=Accuracy))+
  geom_point(aes(y=Accuracy, color=Accuracy), size=2)+
  scale_color_gradient(low="gold", high="purple") + 
  labs(title="Kobe's accuracy by range", x="Zone", y="Accuracy in %")
shot_range_accuracy

#Amount of Kobe's shots by type
shot_type_amount <-
  ggplot(kobe_data, aes(x=(combined_shot_type))) +
  geom_bar(fill="goldenrod1") +
  labs(title="Amount of Kobe's shots by type", x="Shot Type", y="Amount")
shot_type_amount

#Kobe's Makes and Misses
shots <- ggplot(kobe_data) + geom_point(
  aes(x=loc_x, y=loc_y,color=factor(shot_made_flag)), alpha=0.5) +
  labs(title="Kobe's career shot attempts", color="Make or Miss") + 
  scale_color_manual(labels=c("Miss", "Make"), values=c("gold","purple"))+
  theme_dark() + theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank())
shots

####################
library(viridis)
shots2<-kobe_data %>% mutate(binx = floor(loc_x/3), biny = floor(loc_y/8)) %>% group_by(binx,biny) %>%
  summarise(accuracy = mean(shot_made_flag)*100)
ggplot(shots2)+geom_hex(aes(binx,biny,fill=accuracy),stat="identity")+scale_fill_viridis(option="magma")+
  labs(title="Kobe's Heat Map", fill="Accuracy (%)")+theme_gray()+theme(axis.title.x = element_blank(),
                                                                        axis.title.y = element_blank(),
                                                                        axis.text.x = element_blank(),
                                                                        axis.text.y = element_blank(),
                                                                        axis.ticks.x = element_blank(),
                                                                        axis.ticks.y = element_blank())

shots2 <- ggplot(kobe_data) + geom_bin2d(
  aes(x=loc_x, y=loc_y,color=factor(shot_made_flag)), alpha=0.5) +
  labs(title="Kobe's career shot attempts", color="Make or Miss") + 
  scale_color_manual(labels=c("Miss", "Make"), values=c("gold","purple"))+
  theme_dark() + theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
                       axis.title.x = element_blank(), axis.title.y = element_blank())
shots2
####################

#Types of Shots
shot_types <- ggplot(kobe_data) + geom_point(
  aes(x=loc_x, y=loc_y,color=factor(shot_made_flag)), alpha=0.5) +
  labs(title="Kobe's shot attempts by type", color="Make or Miss") + 
  scale_color_manual(labels=c("Miss", "Make"), values=c("gold","purple"))+
  theme_dark() + theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
                       axis.title.x = element_blank(), axis.title.y = element_blank())+
  facet_wrap(~combined_shot_type)
shot_types

#Kobe's Accuracy by season
accuracy_by_season <- kobe_data %>% group_by(season) %>%
  summarize(Accuracy=mean(shot_made_flag*100, na.rm=TRUE)) %>%
  ggplot(aes(x=season, y=Accuracy, group=1)) +
  geom_line(aes(color=Accuracy))+
  geom_point(aes(y=Accuracy, color=Accuracy), size=2)+
  scale_color_gradient(low="gold", high="purple") + 
  labs(title="Kobe's accuracy by season", x="Season", y="Accuracy in %")+
  theme_minimal() + theme(axis.text.x = element_text(angle=90), 
                          legend.position = "none")
accuracy_by_season

#Kobe's shot accuracy by type during the regular season vs the playoffs
shot_accuracy_reg_playoffs <- kobe_data %>% group_by(combined_shot_type, playoffs) %>%
  summarize(Accuracy=mean(shot_made_flag*100, na.rm=TRUE)) %>%
  ggplot()+ geom_col(aes(combined_shot_type, Accuracy, fill=factor(playoffs)),position = "dodge")+
  labs(title="Kobe's accuracy by shot type (regular season vs playoffs)", x="Shot Type", y="Accuracy (%)", 
       fill="Regular Season vs Playoffs")+
  scale_fill_manual(labels=c("Regular Season", "Playoffs"), values=c("purple","goldenrod1"))+
  theme_minimal()
shot_accuracy_reg_playoffs

#Shot chart
library(grid)
library(jpeg)
library(RCurl)

url.court <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(url.court)), width=unit(1,"npc"),height=unit(1,"npc"))

ggplot(kobe_data, aes(x=loc_x, y=loc_y)) + annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(color = shot_zone_basic, shape = event_type, alpha=0.05)) + 
  xlim(-250, 250) + ylim(-50,420)

#heatmap v2
library(hexbin)

ggplot(kobe_data, aes(x=loc_x, y=loc_y)) +
  
  geom_point(aes(color=event_type, alpha=0.5), size=0.25)+
  scale_color_manual(values=c("#FDB927","#552583")) +
  guides(alpha = F, size = F) +
  xlim(-250, 250) + ylim(-50,420) + 
  geom_rug(alpha = 0.2) +
  coord_fixed()




#Using hexbin will compute counts seperately
# this is helpful! https://unconj.ca/blog/custom-hexbin-functions-with-ggplot.html
# it computes the cell index outside of the data here, and then
# summarizes by cell index to get the values want to plot

hb <- hexbin(kobe_data$loc_x,kobe_data$loc_y,xbins=24,IDs=T)
coords<-as_tibble(hcell2xy(hb)) %>% mutate(cell=hb@cell)
kobe_data$cell <- hb@cID

# Now summarize by cell to get what I want
kb2<-kobe_data %>% group_by(cell) %>% summarise(accuracy = mean(shot_made_flag),count=n())

#pull back the coordinates we calculated with hexbin
kb2<-kb2 %>% right_join(coords, by="cell")


library(viridis)

ggplot(kb2)+
  annotation_custom(court, -250, 250, -52, 420)+
  geom_hex(aes(x=x, y=y, fill=accuracy,alpha=count), stat="identity") +
  scale_fill_gradient(low="#552583",high="#FDB927")+
  scale_alpha_continuous(trans="log2")+
  coord_fixed()


ggplot(kb2)+
  annotation_custom(court, -250, 250, -52, 420)+
  geom_hex(aes(x=x, y=y, fill=accuracy,alpha=count), stat="identity") +
  scale_fill_viridis(option = "plasma")+
  scale_alpha_continuous(trans="log2")+
  coord_fixed()+
  theme(panel.background = element_rect(fill="white"))


