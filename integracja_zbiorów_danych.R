# program zbierający do kupy wszystkie csv-ki z wynikami
# wyborów do sejmu i wyrzucający je do jednej tabelki do pliku: WYBORY_DO_SEJMU_ZBIORCZO.csv.
# Następnie przeprowadzane jest częściowe uspójnienie nazw partii pomiędzy wyborami
# ( na przykład KW Prawo i Sprawiedliwość = Prawo i Sprawiedliwość = PiS).
# Tak poprawione dane eksportuje się do pliku WYBORY_DO_SEJMU_ZBIORCZO2.csv






library(dplyr)
library(readxl)
library(reshape2)





sejm2005.turnout<-read.csv(
  "D:/dane magisterka/2005parlament/crawling po gminie/frekwencja_wybory_2005_po_gminach.csv", encoding = 'UTF-8',
  colClasses = c("numeric","factor","character","character","numeric","numeric","numeric","numeric"))
sejm2005<-read.csv(
  "D:/dane magisterka/2005parlament/crawling po gminie/wybory_do_sejmu_2005_po_gminach_wyniki.csv", encoding = 'UTF-8',
  colClasses = c("numeric","factor","factor","character","character","character","numeric","numeric"))

sejm2007.turnout<-read.csv(
  "D:/dane magisterka/2007parlament/crawling-po_gminie/frekwencja_wybory_2007_po_gminach.csv", encoding = 'UTF-8',
  colClasses = c("numeric","factor","character","character","numeric","numeric","numeric","numeric"))
sejm2007<-read.csv(
  "D:/dane magisterka/2007parlament/crawling-po_gminie/wybory_do_sejmu_2007_po_gminach_wyniki.csv", encoding = 'UTF-8',
  colClasses = c("numeric","factor","factor","character","character","character","numeric","numeric"))
sejm2011.turnout<-read.csv(
  "D:/dane magisterka/2011parlament/na_poziomie_gminy/frekwencja_wybory_2011_po_gminach.csv", encoding = 'UTF-8',
  colClasses = c("numeric","factor","character","character","character","character","character","character"))

sejm2011<-read.csv(
  "D:/dane magisterka/2011parlament/na_poziomie_gminy/wybory_do_sejmu_2011_po_gminach_wyniki.csv", encoding = 'UTF-8',
  colClasses = c("numeric","factor","factor","character","character","character","character","character"))

sejm2015<-read_excel("D:/dane magisterka/2015parlament/sejm/2015-gl-lis-gm.xls")

sejm2019<-read_excel("D:/dane magisterka/2019parlament/wyniki_gl_na_listy_po_gminach_sejm.xlsx", col_types = c("Kod TERYT"="text"))


################### dopasowanie nowych wyborów do formatu starych ########################
sejm2015_conversion<-melt(data = sejm2015, id.vars = c("TERYT","Gmina"), measure.vars = 27:43)
# pojawia się problem z powiatem :/

sejm2019_conversion<-melt(data = sejm2019, id.vars = c("Kod TERYT","Gmina","Powiat"), measure.vars = 27:36)

sejm2019_conversion$year <- 2019
sejm2019_conversion$elections<-"sejm"
sejm2019_conversion$value<- as.numeric(sejm2019_conversion$value)

sejm2019_conversion_final <- sejm2019_conversion%>%
  group_by(Gmina)%>%
  mutate(percentage = 100*value/sum(value, na.rm = T))
  
colnames(sejm2019_conversion_final)<-c("teryt_code","gmina","powiat","political_party","n_votes","X.U.FEFF.year","elections",
                                       "percentage")

# w pliku terytki sa powiaty
terytki<-read_excel("D:/dane magisterka/terytki.xls")
sejm2015_conversion_w_powiat<-left_join(sejm2015_conversion,terytki, by = "TERYT")[,c(-5,-6)]
sejm2015_conversion_w_powiat$year<-2015
sejm2015_conversion_w_powiat$elections<-"sejm"
sejm2015_conversion_final <- sejm2015_conversion_w_powiat%>%
  group_by(Gmina.x)%>%
  mutate(percentage = 100*value/sum(value, na.rm = T))

colnames(sejm2015_conversion_final)<-c("teryt_code","gmina","political_party","n_votes","powiat","X.U.FEFF.year","elections",
                                       "percentage")


#2011 - konwersja na numerica
sejm2011$n_votes<-as.numeric(gsub('[[:space:]]','',sejm2011$n_votes))
sejm2011$percentage<-as.numeric(gsub(',','.',sejm2011$percentage))

big.ass.table<-bind_rows(sejm2005,sejm2019_conversion_final,sejm2007,sejm2011,sejm2015_conversion_final)

test<-big.ass.table%>%
  group_by(X.U.FEFF.year,political_party)%>%
  summarise(glosy_partii = sum(n_votes,na.rm = T))%>%
  group_by(X.U.FEFF.year)%>%
  mutate(wynik_partii = 100*glosy_partii/sum(glosy_partii))




colnames(big.ass.table)<-c("YEAR","ELECTIONS","TERYT","POWIAT","GMINA","POLITICAL_PARTY","VOTES","VOTES_PERCENTAGE")

write.csv(big.ass.table, file = "D:/dane magisterka/WYBORY_DO_SEJMU_ZBIORCZO.csv",row.names = F)


big.ass.table[which(
  grepl("SLD",big.ass.table$POLITICAL_PARTY) |
    grepl("Sojusz",big.ass.table$POLITICAL_PARTY)|
    grepl("SOJUSZ",big.ass.table$POLITICAL_PARTY)),]$POLITICAL_PARTY<-"SLD"
big.ass.table[which(
  grepl("Prawo i",big.ass.table$POLITICAL_PARTY) |
    grepl("PRAWO I",big.ass.table$POLITICAL_PARTY)),]$POLITICAL_PARTY<-"PiS"
big.ass.table[which(
  grepl("Platforma O",big.ass.table$POLITICAL_PARTY) |
    grepl("KOALICJA O",big.ass.table$POLITICAL_PARTY) |
    grepl("PLATFORMA O",big.ass.table$POLITICAL_PARTY)),]$POLITICAL_PARTY<-"PO"
big.ass.table[which(
  grepl("Polskiego Stronn",big.ass.table$POLITICAL_PARTY) |
    grepl("POLSKIE STRO",big.ass.table$POLITICAL_PARTY) |
    grepl("Polskie Stro",big.ass.table$POLITICAL_PARTY)),]$POLITICAL_PARTY<-"PSL"
big.ass.table[which(
  grepl("KONFEDERACJA",big.ass.table$POLITICAL_PARTY) |
    grepl("Korwin",big.ass.table$POLITICAL_PARTY) |
    grepl("KORWiN",big.ass.table$POLITICAL_PARTY)),]$POLITICAL_PARTY<-"KORWIN"
big.ass.table[which(
  grepl("Samoobrona R",big.ass.table$POLITICAL_PARTY) |
    grepl("Samoobrona A",big.ass.table$POLITICAL_PARTY)),]$POLITICAL_PARTY<-"SAMOOBRONA"
big.ass.table[which(
  grepl("MNIEJSZOŚĆ NIEMIECKA",big.ass.table$POLITICAL_PARTY) |
    grepl("Mniejszość Niemiecka",big.ass.table$POLITICAL_PARTY)),]$POLITICAL_PARTY<-"SAMOOBRONA"


write.csv(big.ass.table, file = "D:/dane magisterka/WYBORY_DO_SEJMU_ZBIORCZO2.csv",row.names = F)


partie<-big.ass.table%>%filter(POLITICAL_PARTY=="PSL")%>%group_by(POLITICAL_PARTY, YEAR)%>%summarise(n=n())
partie<-big.ass.table%>%filter(YEAR==2019)%>%group_by(POLITICAL_PARTY, YEAR)%>%summarise(n=n())




