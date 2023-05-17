library(tidyverse)
library(readxl)
library(ggplot2)
library(sciplot)
library(ggmap)
library(mapdata)
library(readr)
rm(list = ("")) 

m=read.csv("continuous_grazing.csv")
str(m) 

m$day<-as.numeric(as.character(m$day)) #Change structure from numeric to character

m1<-m%>% #new column for grazing rate
  mutate(grazing_rate=kelp_wt_start- kelp_wt_end)

m2<-m1 [-c(4),] #delete column 4 error 

m2<-m2 %>% #new column for grazing by size
  mutate(grazing_by_size=(grazing_rate)/urch_diam)

m2<-m2%>% #new column for grazing by weight
  mutate(grazing_by_weight=(grazing_rate)/urch_weight)

m2%>% #plot with grazing rate on y and urchin id on x-axis
  ggplot(mapping = aes(x=urchin_id,y=grazing_rate, color=day))+ geom_point()

m2%>% #plot with  grazing by size on y and urchins id on x-axis
  ggplot(mapping = aes(x=urchin_id,y=grazing_by_size ,color=day))+ geom_point()

m2%>% #plot with  grazing by weight on y and urchins id on x-axis
  ggplot(mapping = aes(x=urchin_id,y=grazing_by_weight ,color=day))+ geom_point()
###-----Grouping trials to get average and standard error-------

#group by grazing by size
m3<-m2%>% 
  group_by(urchin_id)%>%
  summarize(MeanGrazing=mean(grazing_by_size),stdGrazing=sd(grazing_by_size))

#group by grazing rate
m4<-m2%>% 
  group_by(urchin_id)%>%
  summarize(MeanGrazing=mean(grazing_rate),stdGrazing=sd(grazing_rate))

#group by grazing by weight
m5<-m2%>% 
  group_by(urchin_id)%>%
  summarize(MeanGrazing=mean(grazing_by_weight),stdGrazing=sd(grazing_by_weight))

#------------ggplot with average of 3 trials with std -------------

m3%>% #grazing by size
  ggplot(mapping = aes(x=urchin_id,y=MeanGrazing))+ geom_point()+
  geom_errorbar(aes(ymin = MeanGrazing - stdGrazing, ymax = MeanGrazing + stdGrazing))+
  theme(axis.text.x = element_text(angle = 90)) #rotates labels on x-xis


m4%>% #grazing rate
  ggplot(mapping = aes(x=urchin_id,y=MeanGrazing))+ geom_point()+
  geom_errorbar(aes(ymin = MeanGrazing - stdGrazing, ymax = MeanGrazing + stdGrazing))+
  theme(axis.text.x = element_text(angle = 90)) #rotates labels on x-xis


m5%>% #grazing by weight
  ggplot(mapping = aes(x=urchin_id,y=MeanGrazing))+ geom_point()+
  geom_errorbar(aes(ymin = MeanGrazing - stdGrazing, ymax = MeanGrazing + stdGrazing))+
  theme(axis.text.x = element_text(angle = 90)) #rotates labels on x-xis
