data<-read.csv2("temperature.csv",header=T)

data$date_time<-as.POSIXct(data$date_time,format="%Y-%m-%d %H:%M")
data$date<-as.POSIXct(data$date,format="%Y-%m-%d")
data$time<-as.POSIXct(data$time,format="%H:%M:%S")
data$temp<-as.numeric(data$temp)

#plot date and time against temperature
plot(data$date_time,data$temp)
