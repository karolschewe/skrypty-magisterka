library(dplyr)

# na poczatku wczytuje sie dane z pliku, a w nastepnej kolejnosci usuwane sa nieprzyporzadkowane partie
# potem nastepuje renormalizacja do 100%, aby nie spowodowala ona przeklamania wypelnienie powinno byc na poziomie ok. 99%


wyniki<-read.csv("D:/dane magisterka/WYBORY_DO_SEJMU_KONSERWATYZM.csv")

# grupowanie po swiatopogladzie
wyniki.binary<-wyniki%>%
  group_by(YEAR,ELECTIONS,TERYT,POWIAT,GMINA,przyporzadkowanie)%>%
  summarise(VOTES_IN_GROUP = sum(VOTES,na.rm = T), PERCENTAGE_IN_GROUP = sum(VOTES_PERCENTAGE,na.rm = T))

#nieprzyporzadkowani
wyniki.unknown<-wyniki.binary%>%
  filter(przyporzadkowanie%in%c("???","????"))%>%
  group_by(YEAR,ELECTIONS,TERYT,POWIAT,GMINA)%>%
  summarise(UNKNOWN_VOTES = sum(VOTES_IN_GROUP,na.rm = T), UNKNOWN_PERCENTAGE = sum(PERCENTAGE_IN_GROUP,na.rm = T))
# suma w gminie
wyniki.gmina<-wyniki.binary%>%
  group_by(YEAR,ELECTIONS,TERYT,POWIAT,GMINA)%>%
  summarise(SUM_VOTES = sum(VOTES_IN_GROUP,na.rm = T))
#renormalizacja
wyniki.to.be.renormalised<-left_join(wyniki.binary, wyniki.unknown, by = c("YEAR","ELECTIONS","TERYT","POWIAT","GMINA"))%>%
  left_join(wyniki.gmina,by = c("YEAR","ELECTIONS","TERYT","POWIAT","GMINA"))%>%
  filter(!przyporzadkowanie%in%c("???","????"))
wyniki.to.be.renormalised$UNKNOWN_VOTES[is.na(wyniki.to.be.renormalised$UNKNOWN_VOTES)]<-0
wyniki.to.be.renormalised$UNKNOWN_PERCENTAGE[is.na(wyniki.to.be.renormalised$UNKNOWN_PERCENTAGE)]<-0


wyniki.final<-wyniki.to.be.renormalised%>%
  mutate(APPROX_VOTES = VOTES_IN_GROUP/(SUM_VOTES-UNKNOWN_VOTES)*SUM_VOTES)%>%
  mutate(APPROX_PERCENTAGE = APPROX_VOTES/SUM_VOTES)%>%
  select(YEAR,ELECTIONS,TERYT,POWIAT,GMINA,przyporzadkowanie,APPROX_VOTES,APPROX_PERCENTAGE)


write.csv(x = wyniki.final, file = "D:/dane magisterka/WYBORY_DO_SEJMU_KONSERWATYZM_renormalizowane.csv", row.names = F)
