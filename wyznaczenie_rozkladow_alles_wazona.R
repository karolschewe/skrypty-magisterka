# program rysujący rozkłady odchyleń poparć w gminie od średniego poparcia w Polsce w celu sprawdzenia (na oko) 
# czy ich rozkłady są normalne – potrzebne jest to do późniejszego wyznaczenia korelacji





library(dplyr)


plotted.year<-2005





























if(!exists("wyniki")){wyniki<-read.csv("D:/dane magisterka/WYBORY_DO_SEJMU_ZBIORCZO2.csv")}

wyniki.year<-wyniki[which(wyniki$YEAR == plotted.year),]

wszystkie_glosy<-sum(wyniki.year$VOTES,na.rm = T)

df.wagi<-wyniki.year%>%
  group_by(GMINA)%>%
  summarise(waga.gmina = sum(VOTES,na.rm = T))
wyniki.wagi<-left_join(wyniki.year, df.wagi, by="GMINA")
wyniki.wagi$udzial.posredni<-(wyniki.wagi$VOTES_PERCENTAGE*wyniki.wagi$waga.gmina)/wszystkie_glosy


df.mean<-wyniki.wagi%>%
  group_by(POLITICAL_PARTY)%>%
  summarise(mean.party = sum(udzial.posredni, na.rm = T))%>%
  filter(mean.party > 1)


df.dist<-inner_join(wyniki.year, df.mean, by ="POLITICAL_PARTY")

df.dist$dist.from.mean<-df.dist$VOTES_PERCENTAGE-df.dist$mean.party

ile.partii<-nrow(df.mean)

if(ile.partii < 7){
  par(mfrow=c(2,3))
}else if (ile.partii < 10)
{
  par(mfrow=c(3,3))
}else if(ile.partii<13){
  par(mfrow=c(4,3))
}else{par(mfrow=c(4,4))}

for(i in df.mean$POLITICAL_PARTY)
{
  span.min<-min(min(df.dist$dist.from.mean[df.dist$POLITICAL_PARTY == i],na.rm = T)-1,-30)
  span.max<-max(max(df.dist$dist.from.mean[df.dist$POLITICAL_PARTY == i],na.rm = T)+1,30)
  
  hist(df.dist$dist.from.mean[df.dist$POLITICAL_PARTY == i], 
       main = paste0("Distance from mean distribution\n ",i,"\n",plotted.year," Sejm elections"), 
       xlab = "v-<v>[%]",  cex.main =1.6, breaks = seq(from = span.min, to = span.max, by = 1)  )
}