library(dplyr)

year<-2007
if(!exists("wyniki")){wyniki<-read.csv("D:/dane magisterka/WYBORY_DO_SEJMU_KONSERWATYZM_renormalizowane.csv",colClasses = c("TERYT"= "factor"))}



#macierz konserwa


wyniki.konserwa<-wyniki%>%
  filter(YEAR == year)%>%
  filter(przyporzadkowanie == "KONSERWATYZM")



calculate.matrix.row<-function(df.wyniki,idx){
  row<-list(df.wyniki$TERYT[idx])
  our.votes<-df.wyniki$APPROX_PERCENTAGE[idx]
  for(i in 1:nrow(df.wyniki))
  {
    row<-append(row, our.votes*df.wyniki$APPROX_PERCENTAGE[i])
  }
  return(row)
}

calculate.matrix<-function(dataframka){
  for(wiersz in 1:nrow(dataframka))
  {
    assign(paste0("",wiersz),calculate.matrix.row(dataframka,wiersz))
    
  }
  szajz<-as.character(1:nrow(dataframka))
  szajz2<-mget(szajz)
  dffff<-do.call(rbind.data.frame,szajz2)
  colnames(dffff)<-c("TERYT",as.character(dffff[,1]))
  return(dffff)
}

kkkk<-calculate.matrix(wyniki.konserwa)
write.csv(kkkk,paste0("D:/dane magisterka/wyznaczenie korelacji/iloczyny/iloczyny_KONSERWATYZM",year,".csv"),row.names = F)


wyniki.reszta<-wyniki%>%
  filter(YEAR == year)%>%
  filter(przyporzadkowanie == "ANTYKONSERWATYZM")


aaaa<-calculate.matrix(wyniki.reszta)

write.csv(aaaa,paste0("D:/dane magisterka/wyznaczenie korelacji/iloczyny/iloczyny_ANTYKONSERWATYZM",year,".csv"),row.names = F)


