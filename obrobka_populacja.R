library(dplyr)
library(readxl)
library(data.table)


woj.dolnoslaskie<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 2,range = "A5:C999",col_names = T)
woj.kujawsko.pom<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 3,range = "A5:C999",col_names = T)
woj.lubelskie<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 4,range = "A5:C999",col_names = T)
woj.lubuskie<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 5,range = "A5:C999",col_names = T)
woj.lodzkie<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 6,range = "A5:C999",col_names = T)
woj.malopolskie<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 7,range = "A5:C999",col_names = T)
woj.mazowieckie<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 8,range = "A5:C999",col_names = T)
woj.opolskie<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 9,range = "A5:C999",col_names = T)
woj.podkarpackie<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 10,range = "A5:C999",col_names = T)
woj.podlaskie<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 11,range = "A5:C999",col_names = T)
woj.pomorskie<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 12,range = "A5:C999",col_names = T)
woj.slaskie<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 13,range = "A5:C999",col_names = T)
woj.swietokrzyskie<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 14,range = "A5:C999",col_names = T)
woj.warm.mazurskie<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 15,range = "A5:C999",col_names = T)
woj.wielkopolskie<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 16,range = "A5:C999",col_names = T)
woj.zachodniopomorskie<-read_xls("D:/dane magisterka/populacje gmin/LUD_bilans_ludnosci_31-12-2011.xls",sheet = 17,range = "A5:C999",col_names = T)





alles<-rbindlist(mget(grep("woj",ls(),value = TRUE)))
colnames(alles)[1]<-"GMINA"
filtered.alles<-alles%>%
  filter(!is.na(`Symbol terytorialny`))%>%
  filter(!is.na(Ogółem))%>%
  filter(!substr(`Symbol terytorialny`,5,6)=="")%>%
  filter(!substr(GMINA,1,1) == "D")%>%
  mutate(TERYT = substr(x = `Symbol terytorialny`,1,6))


to.be.exported<-data.frame(TERYT = factor(filtered.alles$TERYT), POPULACJA = filtered.alles$Ogółem)

write.csv(to.be.exported,"D:/dane magisterka/populacje gmin/populacje2012.csv",row.names = F)
read.csv("D:/dane magisterka/populacje gmin/populacje2012.csv", colClasses = c("factor","numeric"))
