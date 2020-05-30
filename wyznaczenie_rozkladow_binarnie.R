library(dplyr)

# program rysujący rozkłady odchyleń poparć w gminie od średniego poparcia w Polsce w celu sprawdzenia (na oko) 
# czy ich rozkłady są normalne – potrzebne jest to do późniejszego wyznaczenia korelacji















plotted.year<-2019



if(!exists("wyniki")){wyniki<-read.csv("D:/dane magisterka/WYBORY_DO_SEJMU_KONSERWATYZM_renormalizowane.csv")}



wyniki.year<-wyniki[which(wyniki$YEAR == plotted.year),]




df.mean<-wyniki.year%>%
  group_by(przyporzadkowanie)%>%
  summarise(mean.party = mean(APPROX_PERCENTAGE, na.rm = T))


df.dist<-inner_join(wyniki.year, df.mean, by ="przyporzadkowanie")

df.dist$dist.from.mean<-(df.dist$APPROX_PERCENTAGE-df.dist$mean.party)*100


par(mfrow=c(1,2))


for(i in df.mean$przyporzadkowanie)
{
  span.min<-min(min(df.dist$dist.from.mean[df.dist$przyporzadkowanie == i],na.rm = T)-1,-30)
  span.max<-max(max(df.dist$dist.from.mean[df.dist$przyporzadkowanie == i],na.rm = T)+1,30)
  tt<-round(shapiro.test(df.dist$dist.from.mean[df.dist$przyporzadkowanie == i])$p.value, digits = 4)
  
  hist(df.dist$dist.from.mean[df.dist$przyporzadkowanie == i], 
       main = paste0("Distance from mean distribution\n ",i,"\n",plotted.year," Sejm elections"), 
       xlab = "v-<v>[%]",  cex.main =1.4, breaks = seq(from = span.min, to = span.max, by = 1)  )
  mtext(paste0("shapiro-wilk p-value:",tt), 2, adj=1, line=2)
}

print(shapiro.test(df.dist$dist.from.mean[df.dist$przyporzadkowanie == "KONSERWATYZM"]))

