# program wykonuje mapowanie binarne KONSERWATYZM/ANTYKONSERWATYZM na podstawie pliku partie.xlsx
# nastepnie dokonuje sie sprawdzenia czy mapowanie pokrywa wystarczajaca czesc elektoratu (min 98.5%)





library(dplyr)
library(readxl)






wyniki<-read.csv("D:/dane magisterka/WYBORY_DO_SEJMU_ZBIORCZO2.csv",colClasses = c("TERYT"= "factor"))

slownik.mapowanie<-read_xlsx("partie.xlsx")























wyniki.konserwatyzm<-left_join(wyniki,slownik.mapowanie, by = c("POLITICAL_PARTY" = "partia"))



# test przypisania
sumy<-wyniki.konserwatyzm%>%
  group_by(YEAR,przyporzadkowanie)%>%
  summarise(poparcie=sum(VOTES, na.rm = T))

procenty<-sumy%>%
  group_by(YEAR)%>%
  mutate(procent = poparcie/sum(poparcie))


# kto jest w partiach ??? w 2019r?

wyniki.konserwatyzm%>%
  filter(YEAR==2015)%>%
  filter(przyporzadkowanie=="???")%>%
  distinct(POLITICAL_PARTY)


# niesklasyfikowane opcje na poziomie max 1% -- eksportuje do csv


# braki na poziomie gminy
sumy.gmina<-wyniki.konserwatyzm%>%
  group_by(YEAR,TERYT,przyporzadkowanie)%>%
  summarise(poparcie=sum(VOTES, na.rm = T))

procenty.gmina<-sumy.gmina%>%
  group_by(YEAR,TERYT)%>%
  mutate(procent = poparcie/sum(poparcie))%>%
  filter(przyporzadkowanie %in% c("???","????"))%>%
  arrange(desc(procent))%>%
  filter(procent>0.05)
# ciagu 15 lat tylko w 200 przypadkach na 143000 suma niesklasyfikowanych opcji w gminie przekroczyla prog wyborczy
write.csv(wyniki.konserwatyzm,"D:/dane magisterka/WYBORY_DO_SEJMU_KONSERWATYZM.csv", row.names = F)






