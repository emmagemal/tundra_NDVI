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
ggplot(data,aes(date_time,temp,color=elev_cat))+geom_point()+facet_grid(~site)
