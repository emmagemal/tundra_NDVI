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
install.packages("ggplot2")
library("ggplot2")

#point
f1 <- function(x) c(Mean = mean(x), Max = max(x),Min=min(x), SD = sd(x))
mean<-do.call(data.frame, aggregate(temp~site+elev_cat, data, f1))

mean$elev_cat <- relevel(mean$elev_cat,"H")
mean$elev_cat <- relevel(mean$elev_cat,"MH")
mean$elev_cat <- relevel(mean$elev_cat,"M")
mean$elev_cat <- relevel(mean$elev_cat,"ML")
mean$elev_cat <- relevel(mean$elev_cat,"L")

ggplot(mean,aes(elev_cat,temp.Mean))+geom_point(color="red")+geom_point(data=mean,aes(elev_cat,temp.Max),color="blue")+geom_point(data=mean,aes(elev_cat,temp.Min),color="purple")+geom_errorbar(aes(ymin=temp.Mean-temp.SD, ymax=temp.Mean+temp.SD),color="red", width = 0.05)+geom_errorbar(aes(ymin=temp.Max-temp.SD, ymax=temp.Max+temp.SD),color="blue", width = 0.05)+geom_errorbar(aes(ymin=temp.Min-temp.SD, ymax=temp.Min+temp.SD),color="purple", width = 0.05)+facet_grid(~site)


#boxplot
data$elev_cat <- relevel(data$elev_cat,"H");data$elev_cat <- relevel(data$elev_cat,"MH");data$elev_cat <- relevel(data$elev_cat,"M");data$elev_cat <- relevel(data$elev_cat,"ML");data$elev_cat <- relevel(data$elev_cat,"L")

ggplot(data,aes(elev_cat,temp,color=elev_cat))+geom_boxplot()+facet_grid(~site)

#model
model<-aov(data=data,temp~elev_cat*site); summary(model)

