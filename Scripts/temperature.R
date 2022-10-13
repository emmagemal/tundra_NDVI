data<-read.csv2("temperature.csv",header=T)

data$date_time<-as.POSIXct(data$date_time,format="%Y-%m-%d %H:%M")
data$date<-as.POSIXct(data$date,format="%Y-%m-%d")
data$time<-as.POSIXct(data$time,format="%H:%M:%S")
data$temp<-as.numeric(data$temp)
data$elev_cat<-as.factor(data$elev_cat)
data$site<-as.factor(data$site)

#plot date and time against temperature
plot(data$date_time,data$temp)

#plot date and time against temperature + color
plot(temp~date_time, data=data, pch=20,cex=0.8, col=elev_cat);legend("topleft",legend=levels(data$elev_cat),fill=1:5,cex=0.8)

####N and K separated####
library(tidyverse)
library(ggplot2)

#point
f1 <- function(x) c(Mean = mean(x), Max = max(x),Min=min(x), SD = sd(x))
mean<-do.call(data.frame, aggregate(temp~site+elev_cat, data, f1))

#relevel so we get the righ order: L, ML, M, MH, H
mean$elev_cat <- relevel(mean$elev_cat,"H"); mean$elev_cat <- relevel(mean$elev_cat,"MH"); mean$elev_cat <- relevel(mean$elev_cat,"M"); mean$elev_cat <- relevel(mean$elev_cat,"ML"); mean$elev_cat <- relevel(mean$elev_cat,"L")

#Plot showing mean max, mean and mean low
ggplot(mean,aes(elev_cat,temp.Mean))+geom_point(color="red")+geom_point(data=mean,aes(elev_cat,temp.Max),color="blue")+geom_point(data=mean,aes(elev_cat,temp.Min),color="purple")+geom_errorbar(aes(ymin=temp.Mean-temp.SD, ymax=temp.Mean+temp.SD),color="red", width = 0.05)+geom_errorbar(aes(ymin=temp.Max-temp.SD, ymax=temp.Max+temp.SD),color="blue", width = 0.05)+geom_errorbar(aes(ymin=temp.Min-temp.SD, ymax=temp.Min+temp.SD),color="purple", width = 0.05)+facet_grid(~site)


####boxplot - THIS IS THE ONE WE USE####
#relevel so we get the righ order: L, ML, M, MH, H
data$elev_cat <- relevel(data$elev_cat,"H");data$elev_cat <- relevel(data$elev_cat,"MH");data$elev_cat <- relevel(data$elev_cat,"M");data$elev_cat <- relevel(data$elev_cat,"ML");data$elev_cat <- relevel(data$elev_cat,"L")

#plot it and change K and N to Wet and Dry
#is not working :(
data %>%
  mutate(elev_cat = case_when(elev_cat == "K" ~ "Wet",
                              elev_cat == "N" ~ "Dry")) %>% ggplot(data,aes(elev_cat,temp,color=elev_cat))+geom_boxplot()+facet_grid(~site)

#Get max, mean and min temp per day
#Might be used in model
f2 <- function(x) c(Mean = mean(x), Max = max(x),Min=min(x),SD=sd(x))
mean_date<-do.call(data.frame, aggregate(temp~site+elev_cat+date, data, f2))
#relevel
mean_date$elev_cat <- relevel(mean_date$elev_cat,"H"); mean_date$elev_cat <- relevel(mean_date$elev_cat,"MH"); mean_date$elev_cat <- relevel(mean_date$elev_cat,"M"); mean_date$elev_cat <- relevel(mean_date$elev_cat,"ML"); mean_date$elev_cat <- relevel(mean_date$elev_cat,"L")

#model
#anova. If the data is norm dist. I guess it is not
model<-aov(data=data,temp~elev_cat*site); summary(model)
model4<-lm(data=data, temp~elev_cat*site);summary(model4)

#histograms of mean temp per elevation and site
#few data points to say if normally distributed
ggplot(mean_date,aes(x=temp.Mean))+geom_histogram()+facet_grid(~site+elev_cat)+theme_bw()
#Using mean per date 
model2<-aov(data=mean_date, temp.Mean~site*elev_cat)
model3<-lm(data=mean_date, temp.Mean~site*elev_cat);summary(model3)
summary(model2)


Tukey<-TukeyHSD(model2)
plot(Tukey)


