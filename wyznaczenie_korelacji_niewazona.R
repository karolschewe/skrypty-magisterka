library(dplyr)
library(reshape2)
library(tictoc)

year.calculated<-2007










open.my.matrix<-function(df.paths)
{
  colnames(df.paths)[1]<-"from"
  colnames(df.paths)<-sub("X","",colnames(df.paths))
  
  #wracamy do postaci listy krawedzi -> tak bedzie mi prosciej sprawdzic jakosc tych danych
  
  weights.list<-melt(df.paths,id.vars = "from")%>%filter(is.na(value)==FALSE)
  weights.list$from<-as.character(weights.list$from)
  colnames(weights.list)[2]<-"to"
  weights.list$to<-as.character(weights.list$to)
  return(weights.list)
  
}





#WCZYTANIE MACIERZY ODLEGLOSCI
df.geodesics<-read.csv("D:/dane magisterka/wyznaczenie odległości pomiędzy gminami/polaczenia_graf/macierz_odleglosci_cut_teryt.csv",
                       colClasses = c("X" = "factor"))
df.geodesics[,1]<-sub("X","",colnames(df.geodesics)[-1])
df.geodesics.melt<-open.my.matrix(df.geodesics)


#wczytanie iloczynów odleglosci
df.iloczyny<-read.csv(paste0("D:/dane magisterka/wyznaczenie korelacji/iloczyny/iloczyny_KONSERWATYZM",year.calculated,".csv"),
                      colClasses = c("TERYT" = "factor"))
colnames(df.iloczyny)<-sub("X","",colnames(df.iloczyny))
df.iloczyny.melt<-melt(df.iloczyny, id.vars = "TERYT")
df.iloczyny.melt$TERYT<-as.character(df.iloczyny.melt$TERYT)
df.iloczyny.melt$variable<-as.character(df.iloczyny.melt$variable)
#######WYZNACZENIE WARIANCJI W ROZKŁADACH ODLEGŁOŚCI OD ŚREDNIEJ KONSERWATYZM/ANTYKONSERWATYZM############


if(!exists("wyniki")){wyniki<-read.csv("D:/dane magisterka/WYBORY_DO_SEJMU_KONSERWATYZM_renormalizowane.csv",colClasses = c("TERYT"= "factor"))}



wyniki.year<-wyniki[which(wyniki$YEAR == year.calculated),]




wszystkie_glosy<-sum(wyniki.year$APPROX_VOTES,na.rm = T)

df.wagi<-wyniki.year%>%
  group_by(TERYT)%>%
  summarise(waga.gmina = sum(APPROX_VOTES,na.rm = T))
wyniki.wagi<-left_join(wyniki.year, df.wagi, by="TERYT")
wyniki.wagi$udzial.posredni<-(wyniki.wagi$APPROX_PERCENTAGE*wyniki.wagi$waga.gmina)/wszystkie_glosy



df.mean<-wyniki.wagi%>%
  group_by(przyporzadkowanie)%>%
  summarise(mean.party = sum(udzial.posredni, na.rm = T))



df.dist<-inner_join(wyniki.year, df.mean, by ="przyporzadkowanie")

df.dist$dist.from.mean<-(df.dist$APPROX_PERCENTAGE-df.dist$mean.party)*100

variance.KONSERWA<-var(df.dist$APPROX_PERCENTAGE[df.dist$przyporzadkowanie=="KONSERWATYZM"])
variance.RESZTA<-var(df.dist$APPROX_PERCENTAGE[df.dist$przyporzadkowanie=="ANTYKONSERWATYZM"])
meansq.KONSERWA<-mean(df.dist$APPROX_PERCENTAGE[df.dist$przyporzadkowanie=="KONSERWATYZM"],na.rm = T)^2
meansq.RESZTA<-mean(df.dist$APPROX_PERCENTAGE[df.dist$przyporzadkowanie=="ANTYKONSERWATYZM"],na.rm = T)^2


########FUNKCJA WYZNACZAJACA KORELACJE KONSERWATYZM W PROMIENIU R#######################

TERYT.list<-colnames(df.iloczyny)[-1]


calculate.one.correlation.konserwa<-function(terytek,threshold.km){
  df.one.TERYT<-df.geodesics.melt%>%
    filter(from==terytek)%>%
    filter(value<threshold.km)%>%
    filter(value > 0)
  if(nrow(df.one.TERYT)==0)
  {
    return(NA)
  }
  df.one.product<-df.iloczyny.melt%>%filter(TERYT==terytek)
  
  df.one.TERYT.w.products<-left_join(df.one.TERYT,df.one.product,by=c("from"="TERYT","to"="variable"))
  one.TERYT.mean.product<-mean(df.one.TERYT.w.products$value.y,na.rm = T) # sredni iloczyn poparc
  one.teryt.correlation<-(one.TERYT.mean.product-meansq.KONSERWA)/variance.KONSERWA
  return(one.teryt.correlation)
  
  
}
convert.konserwa.to.antikonserwa<-function(corr.konserwa)
{
  (((corr.konserwa*variance.KONSERWA)+meansq.KONSERWA)-meansq.RESZTA)/variance.RESZTA
}
calculate.one.correlation.konserwa(TERYT.list[8],30)
odleglosci<-c(10,15,20,25,30,40,50,75,100,150,200,250,300,400,500)
for (km in odleglosci)
{
  tic(paste0(km,"km"))
  assign(paste0("corr.",km),lapply(TERYT.list, calculate.one.correlation.konserwa, km))
  dput(get(paste0("corr.",km)),paste0("D:/dane magisterka/wyznaczenie korelacji/korelacje/corr_",km,"km.csv"))
  assign(paste0("corr.antykonserwa",km),convert.konserwa.to.antikonserwa(unlist(get(paste0("corr.",km)))))
  dput(get(paste0("corr.antykonserwa",km)),
       paste0("D:/dane magisterka/wyznaczenie korelacji/korelacje/corr_antykonserwatyzm",km,"km.csv"))
  toc()
  
}


