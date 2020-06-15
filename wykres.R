library(dplyr)
odleglosci<-c(10,15,20,25,30,40,50,75,100,150,200,250,300,400,500)

correlations.all<-c()
odleglosci.all<-c()
for(i in odleglosci){
  temporary<-unlist(dget(paste0("D:/dane magisterka/wyznaczenie korelacji/korelacje/corr_",i,"km.csv")))
  correlations.all<-c(correlations.all,temporary)
  odleglosci.all<-c(odleglosci.all, rep(i,length(temporary)))
}

plot(odleglosci.all, correlations.all,log = "x")

ramka.wykres<-data.frame(km = odleglosci.all, corr = correlations.all)
ramka.wykres<-ramka.wykres%>%filter(!is.na(corr))
plot(odleglosci.all,abs(correlations.all)/max(correlations.all, na.rm = T),
     log = "x", xlab = "distance[km]", ylab = "normalised absolute correlation", main = "CORRELATION FUNCTION OF DISTANCE")

means.wykres<-ramka.wykres%>%
  group_by(km)%>%
  summarise(average = mean(corr,na.rm =T))

plot(means.wykres$km,means.wykres$average,
     log = "x", xlab = "distance[km]", ylab = "mean correlation", main = "MEAN CORRELATION FUNCTION OF DISTANCE - CONSERVATISM")