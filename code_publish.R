#Replication Code for 
#Baumgärtner, M., & Klose, J. (2018). Forecasting Exchange Rates with Commodity Prices‐A Global Country Analysis. The World Economy.
#Please note that this code has not been simplified/translated due to time constraints.
#for questions please contact martin.baumgaertner1@gmail.com

options(scipen=999)

library(pacman)
pacman::p_load(lmtest,gridExtra,vrtest,readxl,tseries,urca,zoo,foreign,plm,data.table,lubridate,readr,devtools,Hmisc,tidyverse,widyr,reshape2,forecast,systemfit,
               strucchange,stargazer,dplyr,ForecastCombinations,abind,egcm,xtable,stringi,ISLR,ggplot2,tidyr,
               install = TRUE,update=F)

#################Funktion##############
mse<-function(data1,data2){
  mean((na.trim(data1)-na.trim(data2))^2)
}
rohstoffe<-function(data1){
  dataF<-subset(data1,Year>=1992 & !(RepCo %in% c(28,31,44,48,52,60,84,104,136,156,184,192,212,232,262,275,308,332,344,400,422,446,490,499,500,512,530,533,583,585,591,626,634,659,660,662,670,682,688,711,716,729,736,740,784,795,796,807,842,862,891))) #Daten ab 1992, Länder die ich rausgeschmissen habe

ComNum1<-c(as.numeric(unique(dataF$ComCo))) #Betrachtete Rohstoffe
ComNames1<-unique(dataF$Com)
Rohstoffe<-data.frame(ComNum1,as.character(ComNames1))
Rohstoffe<-Rohstoffe[order(Rohstoffe$ComNum1),]
colnames(Rohstoffe)<-c("ComNum","ComNames")
return(Rohstoffe)
}
handel<-function(data,flow){
  Laender <- read_excel("data/Laender_codes.xlsx")
  
  colnames(Laender)<-c("Land","ID",names(Laender[3:5]))
  
  'Waehrungsraeume'
  AUD<-Laender[Laender$Bemerkung=="AUD"& !is.na(Laender$Bemerkung),]
  Eurozone99<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung),]
  Eurozone01<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung),]
  Eurozone07<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2007"& !is.na(Laender$Bemerkung),]
  Eurozone08<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2007"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2008"& !is.na(Laender$Bemerkung),]
  Eurozone09<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2007"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2008"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2009"& !is.na(Laender$Bemerkung),]
  Eurozone11<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2007"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2008"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2009"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2011"& !is.na(Laender$Bemerkung),]
  Eurozone14<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2007"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2008"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2009"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2011"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2014"& !is.na(Laender$Bemerkung),]
  Eurozone15<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2007"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2008"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2009"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2011"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2014"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2015"& !is.na(Laender$Bemerkung),]
  Eurozonenachruecker<-Laender[Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2007"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2008"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2009"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2011"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2014"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2015"& !is.na(Laender$Bemerkung),]
  CFABCEAO<-Laender[Laender$Bemerkung=="CFA-Franc BCEAO"& !is.na(Laender$Bemerkung),]
  CFABEAC<-Laender[Laender$Bemerkung=="CFA-Franc BEAC"& !is.na(Laender$Bemerkung),]
  CFP<-Laender[Laender$Bemerkung=="CFP-Franc"& !is.na(Laender$Bemerkung),]
  EC<-Laender[Laender$Bemerkung=="EC$"& !is.na(Laender$Bemerkung),]
  CMA<-Laender[Laender$Bemerkung=="CMA"& !is.na(Laender$Bemerkung),]
  Belgien.Luxemburg<-Laender[Laender$Bemerkung2=="BelgiumLuxemburg vor 1999"& !is.na(Laender$Bemerkung2),]
  Franc<-Laender[Laender$Bemerkung2=="Franc vor 1999"& !is.na(Laender$Bemerkung2),]
  Dollar<-Laender[Laender$Fixiert=="Dollar"& !is.na(Laender$Fixiert),]
  Den<-Laender[Laender$Bemerkung=="Denmark"& !is.na(Laender$Bemerkung),]

  dataF<-subset(data,Year>=1992 & !(RepCo %in% c(28,31,44,48,52,60,84,104,116,136,156,184,192,212,232,262,275,308,332,344,400,422,446,490,499,500,512,530,533,583,585,591,626,634,659,660,662,670,682,688,711,716,729,736,740,762,784,795,796,807,842,862,891,Dollar$ID))) #Daten ab 1992, Länder die ich rausgeschmissen habe
  #104 Myanmar hat nur wenige daten
  dataF<-dataF[dataF$FlowCo==flow,] #2 Für Export
  ComNum1<-c(as.numeric(unique(dataF$ComCo))) #Betrachtete Rohstoffe
  ComNames1<-unique(dataF$Com)
  Rohstoffe<-data.frame(ComNum1,as.character(ComNames1))
  Rohstoffe<-Rohstoffe[order(Rohstoffe$ComNum1),]
  colnames(Rohstoffe)<-c("ComNum","ComNames")
  ComNum<-c(as.numeric(unique(Rohstoffe$ComNum))) #Betrachtete Rohstoffe
  ComNames<-unique(Rohstoffe$ComNames) #Rohstoffnamen
  RepNum<-c(unique(dataF$RepCo),1000,1001,1002,1003,1004,1005,1006,1007,1008) #Betrachtete Laender
  RepNames<-c(unique(dataF$Rep),"Eurozone","CFABCEAO","CFABEAC","CMA","Belux","Franc","AUD","CFP","Den") #Laendernamen
  Laender<<-cbind(RepNames,RepNum) #erlaubt das "übersetzen" von Code in Laenderbezeichnung  
  
  
  Year<-c(sort(unique(dataF$Year)))
  NRep<-length(RepNum) #Anzahl Laender +5 Waehrungsraeume
  NCom<-length(ComNum) #Anzahl Rohstoffe
  NYeary<-length(unique(dataF$Year))

  x<-matrix(0,nrow=NCom,ncol=NRep,dimnames = list(ComNames,RepNames)) #Mittelwerte pro Rohstoff pro Land ueber alle Jahre
    for (j in 1:NRep){ print(j)
     for (i in 1:NCom){
       if (is.na(mean(dataF$Value[dataF$ComCo==ComNum[i] & dataF$RepCo==RepNum[j] & dataF$FlowCo==flow]))==FALSE){ #Schaut ob ein Wert vorhanden ist fuer den bestimmten Rohstoff im bestimmten Land
        x[i,j]<-mean(dataF$Value[dataF$ComCo==ComNum[i] & dataF$RepCo==RepNum[j] & dataF$FlowCo==flow])           #Wenn ja dann Wird der Mittelwert ueber alle Jahre in x eingetragen
      } 
    else {x[i,j]=0                                                                                                        #Wenn nicht, dann eine 0
    }
  }}
  
  x[,NRep-8]<-apply(x[,c(paste(Eurozone15$Land))],1,sum)
  x[,NRep-7]<-apply(x[,c(paste(CFABCEAO$Land))],1,sum)
  x[,NRep-6]<-apply(x[,c(paste(CFABEAC$Land))],1,sum)
  x[,NRep-5]<-apply(x[,c(paste(CMA$Land))],1,sum)
  x[,NRep-4]<-apply(x[,c(paste(Belgien.Luxemburg$Land))],1,sum)
  x[,NRep-3]<-apply(x[,c(paste(Franc$Land))],1,sum)
  x[,NRep-2]<-apply(x[,c(paste(AUD$Land))],1,sum)
  x[,NRep-1]  <-apply(x[,c(paste(CFP$Land))],1,sum)
  x[,NRep]  <-apply(x[,c(paste(Den$Land))],1,sum)
  
  Rohstoffe<-Rohstoffe[Rohstoffe$ComNames!="Meat of bovine animals, frozen",]
  levels(Rohstoffe$ComNames)[levels(Rohstoffe$ComNames)=="Meat of bovine animals, fresh or chilled"]<-"Meat of bovine animals, fresh, chilled or frozen"
  colnames(Rohstoffe)<-c("ComNum","ComNames")
  Rohstoffe$ComNames<-factor(Rohstoffe$ComNames)
  Rohstoffe<<-Rohstoffe
  x[rownames(x)=="Meat of bovine animals, fresh or chilled"]<-x[rownames(x)=="Meat of bovine animals, fresh or chilled"] +x[rownames(x)=="Meat of bovine animals, frozen"]
  x<-x[!rownames(x) %in% "Meat of bovine animals, frozen", ]
  rownames(x)[which(rownames(x)=="Meat of bovine animals, fresh or chilled")]<-"Meat of bovine animals, fresh, chilled or frozen"
  NCom<-length(Rohstoffe$ComNum)
  ComNames<-Rohstoffe$ComNames

  xrel<-matrix(0,nrow=NCom,ncol=NRep,dimnames = list(ComNames,RepNames)) #Relative Gewichte an gesamt RohstoffImporten
  for (j in 1:NRep){
    for (i in 1:NCom){
     xrel[i,j]<-x[i,j]/sum(x[,j])
    }}
  return(xrel)
}
handel.netto<-function(data,flow){
  Laender <- read_excel("data/Laender_codes.xlsx")
  colnames(Laender)<-c("Land","ID",names(Laender[3:5]))
  
  'Waehrungsraeume'
  AUD<-Laender[Laender$Bemerkung=="AUD"& !is.na(Laender$Bemerkung),]
  Eurozone99<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung),]
  Eurozone01<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung),]
  Eurozone07<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2007"& !is.na(Laender$Bemerkung),]
  Eurozone08<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2007"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2008"& !is.na(Laender$Bemerkung),]
  Eurozone09<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2007"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2008"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2009"& !is.na(Laender$Bemerkung),]
  Eurozone11<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2007"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2008"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2009"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2011"& !is.na(Laender$Bemerkung),]
  Eurozone14<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2007"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2008"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2009"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2011"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2014"& !is.na(Laender$Bemerkung),]
  Eurozone15<-Laender[Laender$Bemerkung=="Euro 1999"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2007"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2008"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2009"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2011"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2014"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2015"& !is.na(Laender$Bemerkung),]
  Eurozonenachruecker<-Laender[Laender$Bemerkung=="Euro 2001"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2007"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2008"& !is.na(Laender$Bemerkung)|Laender$Bemerkung=="Euro 2009"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2011"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2014"& !is.na(Laender$Bemerkung)| Laender$Bemerkung=="Euro 2015"& !is.na(Laender$Bemerkung),]
  CFABCEAO<-Laender[Laender$Bemerkung=="CFA-Franc BCEAO"& !is.na(Laender$Bemerkung),]
  CFABEAC<-Laender[Laender$Bemerkung=="CFA-Franc BEAC"& !is.na(Laender$Bemerkung),]
  CFP<-Laender[Laender$Bemerkung=="CFP-Franc"& !is.na(Laender$Bemerkung),]
  EC<-Laender[Laender$Bemerkung=="EC$"& !is.na(Laender$Bemerkung),]
  CMA<-Laender[Laender$Bemerkung=="CMA"& !is.na(Laender$Bemerkung),]
  Belgien.Luxemburg<-Laender[Laender$Bemerkung2=="BelgiumLuxemburg vor 1999"& !is.na(Laender$Bemerkung2),]
  Franc<-Laender[Laender$Bemerkung2=="Franc vor 1999"& !is.na(Laender$Bemerkung2),]
  Dollar<-Laender[Laender$Fixiert=="Dollar"& !is.na(Laender$Fixiert),]
  Den<-Laender[Laender$Bemerkung=="Denmark"& !is.na(Laender$Bemerkung),]
  
  dataF<-subset(data,Year>=1992 & !(RepCo %in% c(28,31,44,48,52,60,84,104,116,136,156,184,192,212,232,262,275,308,332,344,400,422,446,490,499,500,512,530,533,583,585,591,626,634,659,660,662,670,682,688,711,716,729,736,740,762,784,795,796,807,842,862,891,Dollar$ID))) #Daten ab 1992, Länder die ich rausgeschmissen habe
  #104 Myanmar hat nur wenige daten
  ComNum1<-c(as.numeric(unique(dataF$ComCo))) #Betrachtete Rohstoffe
  ComNames1<-unique(dataF$Com)
  Rohstoffe<-data.frame(ComNum1,as.character(ComNames1))
  Rohstoffe<-Rohstoffe[order(Rohstoffe$ComNum1),]
  colnames(Rohstoffe)<-c("ComNum","ComNames")
  ComNum<-c(as.numeric(unique(Rohstoffe$ComNum))) #Betrachtete Rohstoffe
  ComNames<-unique(Rohstoffe$ComNames) #Rohstoffnamen
  RepNum<-c(unique(dataF$RepCo),1000,1001,1002,1003,1004,1005,1006,1007,1008) #Betrachtete Laender
  RepNames<-c(unique(dataF$Rep),"Eurozone","CFABCEAO","CFABEAC","CMA","Belux","Franc","AUD","CFP","Den") #Laendernamen
  Laender<<-cbind(RepNames,RepNum) #erlaubt das "übersetzen" von Code in Laenderbezeichnung  
  
  
  Year<-c(sort(unique(dataF$Year)))
  NRep<-length(RepNum) #Anzahl Laender +5 Waehrungsraeume
  NCom<-length(ComNum) #Anzahl Rohstoffe
  NYeary<-length(unique(dataF$Year))

x<-matrix(0,nrow=NCom,ncol=NRep,dimnames = list(ComNames,RepNames)) #Mittelwerte pro Rohstoff pro Land ueber alle Jahre
  for (j in 1:NRep){ print(j)
    L<-RepNum[j]
    data.t<-dataF[dataF$RepCo==L & dataF$FlowCo<3,]
    for (i in 1:NCom){
      d<-NA
      Roh<-Rohstoffe$ComNum[i]
      for(k in 1:25){
        y<-Year[k]
      Export<-data.t$Value[data.t$ComCo==Roh & data.t$Flow=="Export"&data.t$Year==y]
      Import<-data.t$Value[data.t$ComCo==Roh & data.t$Flow=="Import"&data.t$Year==y]
      if(length(Export)==0){Export<-0}
      if(length(Import)==0){Import<-0}
      d[k]<-Export-Import
      
      }
      d[d==0]<-NA
      x[i,j]<-mean(d,na.rm=TRUE)
      if(is.na(x[i,j])){x[i,j]<-0}
    }
  }

x[,NRep-8]<-apply(x[,c(paste(Eurozone15$Land))],1,sum)
x[,NRep-7]<-apply(x[,c(paste(CFABCEAO$Land))],1,sum)
x[,NRep-6]<-apply(x[,c(paste(CFABEAC$Land))],1,sum)
x[,NRep-5]<-apply(x[,c(paste(CMA$Land))],1,sum)
x[,NRep-4]<-apply(x[,c(paste(Belgien.Luxemburg$Land))],1,sum)
x[,NRep-3]<-apply(x[,c(paste(Franc$Land))],1,sum)
x[,NRep-2]<-apply(x[,c(paste(AUD$Land))],1,sum)
x[,NRep-1]<-apply(x[,c(paste(CFP$Land))],1,sum)
x[,NRep]  <-apply(x[,c(paste(Den$Land))],1,sum)

Rohstoffe<-Rohstoffe[Rohstoffe$ComNames!="Meat of bovine animals, frozen",]
levels(Rohstoffe$ComNames)[levels(Rohstoffe$ComNames)=="Meat of bovine animals, fresh or chilled"]<-"Meat of bovine animals, fresh, chilled or frozen"
colnames(Rohstoffe)<-c("ComNum","ComNames")
Rohstoffe$ComNames<-factor(Rohstoffe$ComNames)
Rohstoffe<<-Rohstoffe
x[rownames(x)=="Meat of bovine animals, fresh or chilled"]<-x[rownames(x)=="Meat of bovine animals, fresh or chilled"] +x[rownames(x)=="Meat of bovine animals, frozen"]
x<-x[!rownames(x) %in% "Meat of bovine animals, frozen", ]
rownames(x)[which(rownames(x)=="Meat of bovine animals, fresh or chilled")]<-"Meat of bovine animals, fresh, chilled or frozen"
NCom<-length(Rohstoffe$ComNum)
ComNames<-Rohstoffe$ComNames

xe<-x>=0
xi<-x<=0
xE<-matrix(0,nrow=NCom,ncol=NRep,dimnames = list(ComNames,RepNames))
xI<-matrix(0,nrow=NCom,ncol=NRep,dimnames = list(ComNames,RepNames))
for(j in 1:NRep){
  for (i in 1:NCom){
    ifelse(xe[i,j]==TRUE,xE[i,j]<-x[i,j],0)
    ifelse(xi[i,j]==TRUE,xI[i,j]<-x[i,j],0)
  }
}

xrelnettoE<-matrix(0,nrow=NCom,ncol=NRep,dimnames = list(ComNames,RepNames)) #Relative Gewichte an gesamt RohstoffImporten
xrelnettoI<-matrix(0,nrow=NCom,ncol=NRep,dimnames = list(ComNames,RepNames)) #Relative Gewichte an gesamt RohstoffImporten
for (j in 1:NRep){
  for (i in 1:NCom){
    xrelnettoE[i,j]<-xE[i,j]/sum(xE[,j])
    xrelnettoI[i,j]<-xI[i,j]/sum(xI[,j])
  }}

xrelEnetto<-xrelnettoE
xrelInetto<-xrelnettoI 

if (flow==2){return(xrelEnetto)}
if (flow==1){return(xrelInetto)}
}
loewe<-function(Exporte,Importe,Preismatrix){
  Laender1 <- read_excel("data/Laender_codes.xlsx")
  colnames(Laender1)<-c("Land","ID",names(Laender1[3:5]))
  'Waehrungsraeume'
  AUD<-Laender1[Laender1$Bemerkung=="AUD"& !is.na(Laender1$Bemerkung),]
  Eurozone99<-Laender1[Laender1$Bemerkung=="Euro 1999"& !is.na(Laender1$Bemerkung),]
  Eurozone01<-Laender1[Laender1$Bemerkung=="Euro 1999"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2001"& !is.na(Laender1$Bemerkung),]
  Eurozone07<-Laender1[Laender1$Bemerkung=="Euro 1999"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2001"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2007"& !is.na(Laender1$Bemerkung),]
  Eurozone08<-Laender1[Laender1$Bemerkung=="Euro 1999"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2001"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2007"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2008"& !is.na(Laender1$Bemerkung),]
  Eurozone09<-Laender1[Laender1$Bemerkung=="Euro 1999"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2001"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2007"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2008"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2009"& !is.na(Laender1$Bemerkung),]
  Eurozone11<-Laender1[Laender1$Bemerkung=="Euro 1999"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2001"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2007"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2008"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2009"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2011"& !is.na(Laender1$Bemerkung),]
  Eurozone14<-Laender1[Laender1$Bemerkung=="Euro 1999"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2001"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2007"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2008"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2009"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2011"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2014"& !is.na(Laender1$Bemerkung),]
  Eurozone15<-Laender1[Laender1$Bemerkung=="Euro 1999"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2001"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2007"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2008"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2009"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2011"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2014"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2015"& !is.na(Laender1$Bemerkung),]
  Eurozonenachruecker<-Laender1[Laender1$Bemerkung=="Euro 2001"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2007"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2008"& !is.na(Laender1$Bemerkung)|Laender1$Bemerkung=="Euro 2009"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2011"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2014"& !is.na(Laender1$Bemerkung)| Laender1$Bemerkung=="Euro 2015"& !is.na(Laender1$Bemerkung),]
  CFABCEAO<-Laender1[Laender1$Bemerkung=="CFA-Franc BCEAO"& !is.na(Laender1$Bemerkung),]
  CFABEAC<-Laender1[Laender1$Bemerkung=="CFA-Franc BEAC"& !is.na(Laender1$Bemerkung),]
  CMA<-Laender1[Laender1$Bemerkung=="CMA"& !is.na(Laender1$Bemerkung),]
  Belgien.Luxemburg<-Laender1[Laender1$Bemerkung2=="BelgiumLuxemburg vor 1999"& !is.na(Laender1$Bemerkung2),]
  Franc<-Laender1[Laender1$Bemerkung2=="Franc vor 1999"& !is.na(Laender1$Bemerkung2),]
  CFP<-Laender1[Laender1$Bemerkung=="CFP-Franc"& !is.na(Laender1$Bemerkung),]
  Den<-Laender1[Laender1$Bemerkung=="Denmark"& !is.na(Laender1$Bemerkung),]
  LE<-Preismatrix%*%Exporte
  LI<-Preismatrix%*%Importe

  Loewe1E<-matrix(0,nrow=NYear,ncol=(dim(LE)[2]),dimnames=list(time,each=colnames(LE)))
  for (j in 1:dim(LE)[2]){
    for (i in 1:NYear){ Loewe1E[i,j]<-LE[i,j]/LE[Basis,j]
    }}
  Loewe1I<-matrix(0,nrow=NYear,ncol=(dim(LI)[2]),dimnames=list(time,each=colnames(LI)))
  for (j in 1:dim(LI)[2]){
    for (i in 1:NYear){ Loewe1I[i,j]<-LI[i,j]/LI[Basis,j]
    }}
  
  LoeweblankE<-as.numeric(as.vector(Loewe1E))
  LoeweblankI<-as.numeric(as.vector(Loewe1I))
 
  y<-data.frame(Laender)
  x<-data.frame(colnames(LE))
  colnames(x)<-"RepNames"
  y<-merge(x, y, by = "RepNames", all.x=T,sort=F)

  LoeweE<-data.frame(rep(time,dim(LE)[2]),LoeweblankE,rep(y$RepNum,each=NYear),rep(y$RepNames,each=NYear))
  LoeweI<-data.frame(rep(time,dim(LI)[2]),LoeweblankI,rep(Laender[,2],each=NYear),rep(colnames(LI),each=NYear))
  colnames(LoeweE)<-c("Date","Value","ID","RepName")
  colnames(LoeweI)<-c("Date","Value","ID","RepName")
  LoeweEI<-data.frame(merge(LoeweE,LoeweI,by=c("Date","ID","RepName")))
  colnames(LoeweEI)<-c("Date","ID","Rep","ValueE","ValueI")
  
  #nicht benoetigte Länder rausschmeissen
  for (i in 1:length(Eurozone99$ID)){print(i);
    LoeweEI[LoeweEI$Date>="1999-01-01"&LoeweEI$ID==Eurozone99$ID[i],]$ValueE<-NA;
    LoeweEI[LoeweEI$Date>="1999-01-01"&LoeweEI$ID==Eurozone99$ID[i],]$ValueI<-NA;
  }
  
  for (i in 1:1){print(i);
    LoeweEI[LoeweEI$Date>="2001-01-01"&LoeweEI$ID==setdiff(Eurozone01$ID,Eurozone99$ID)[i],]$ValueE<-NA;
    LoeweEI[LoeweEI$Date>="2001-01-01"&LoeweEI$ID==setdiff(Eurozone01$ID,Eurozone99$ID)[i],]$ValueI<-NA;
  }
  
  for (i in 1:1){print(i);
    LoeweEI[LoeweEI$Date>="2007-01-01"&LoeweEI$ID==setdiff(Eurozone07$ID,Eurozone01$ID)[i],]$ValueE<-NA;
    LoeweEI[LoeweEI$Date>="2007-01-01"&LoeweEI$ID==setdiff(Eurozone07$ID,Eurozone01$ID)[i],]$ValueI<-NA;
  }
  
  for (i in 1:2){print(i);
    LoeweEI[LoeweEI$Date>="2008-01-01"&LoeweEI$ID==setdiff(Eurozone08$ID,Eurozone07$ID)[i],]$ValueE<-NA;
    LoeweEI[LoeweEI$Date>="2008-01-01"&LoeweEI$ID==setdiff(Eurozone08$ID,Eurozone07$ID)[i],]$ValueI<-NA;
  }
  
  for (i in 1:1){print(i);
    LoeweEI[LoeweEI$Date>="2009-01-01"&LoeweEI$ID==setdiff(Eurozone09$ID,Eurozone08$ID)[i],]$ValueE<-NA;
    LoeweEI[LoeweEI$Date>="2009-01-01"&LoeweEI$ID==setdiff(Eurozone09$ID,Eurozone08$ID)[i],]$ValueI<-NA;
  }
  
  for (i in 1:1){print(i);
    LoeweEI[LoeweEI$Date>="2011-01-01"&LoeweEI$ID==setdiff(Eurozone11$ID,Eurozone09$ID)[i],]$ValueE<-NA;
    LoeweEI[LoeweEI$Date>="2011-01-01"&LoeweEI$ID==setdiff(Eurozone11$ID,Eurozone09$ID)[i],]$ValueI<-NA;
  }
  
  for (i in 1:1){print(i);
    LoeweEI[LoeweEI$Date>="2014-01-01"&LoeweEI$ID==setdiff(Eurozone14$ID,Eurozone11$ID)[i],]$ValueE<-NA;
    LoeweEI[LoeweEI$Date>="2014-01-01"&LoeweEI$ID==setdiff(Eurozone14$ID,Eurozone11$ID)[i],]$ValueI<-NA;
  }
  
  for (i in 1:1){print(i);
    LoeweEI[LoeweEI$Date>="2015-01-01"&LoeweEI$ID==setdiff(Eurozone15$ID,Eurozone14$ID)[i],]$ValueE<-NA;
    LoeweEI[LoeweEI$Date>="2015-01-01"&LoeweEI$ID==setdiff(Eurozone15$ID,Eurozone14$ID)[i],]$ValueI<-NA;
  }
  
  for (i in 1:length(CFABCEAO$ID)){print(i);
    LoeweEI[LoeweEI$ID==CFABCEAO$ID[i],]$ValueI<-NA;
    LoeweEI[LoeweEI$ID==CFABCEAO$ID[i],]$ValueE<-NA;
  }
  
  for (i in 1:length(Den$ID)){print(i);
    LoeweEI[LoeweEI$ID==Den$ID[i],]$ValueI<-NA;
    LoeweEI[LoeweEI$ID==Den$ID[i],]$ValueE<-NA;
  }
  
  for (i in 1:length(CFP$ID)){print(i);
    LoeweEI[LoeweEI$ID==CFP$ID[i],]$ValueI<-NA;
    LoeweEI[LoeweEI$ID==CFP$ID[i],]$ValueE<-NA;
  }
  
  for (i in 1:length(CFABEAC$ID)){print(i);
    LoeweEI[LoeweEI$ID==CFABEAC$ID[i],]$ValueI<-NA;
    LoeweEI[LoeweEI$ID==CFABEAC$ID[i],]$ValueE<-NA;
  }
  
  for (i in 1:length(CMA$ID)){print(i);
    LoeweEI[LoeweEI$ID==CMA$ID[i],]$ValueI<-NA;
    LoeweEI[LoeweEI$ID==CMA$ID[i],]$ValueE<-NA;
  }
  
  for (i in 1:length(Franc$ID)){print(i);
    LoeweEI[LoeweEI$ID==Franc$ID[i],]$ValueI<-NA;
    LoeweEI[LoeweEI$ID==Franc$ID[i],]$ValueE<-NA;
  }
  
  for (i in 1:length(Belgien.Luxemburg$ID)){print(i);
    LoeweEI[LoeweEI$ID==Belgien.Luxemburg$ID[i],]$ValueI<-NA;
    LoeweEI[LoeweEI$ID==Belgien.Luxemburg$ID[i],]$ValueE<-NA;
  }
  
  for (i in 1:length(AUD$ID)){print(i);
    LoeweEI[LoeweEI$ID==AUD$ID[i],]$ValueI<-NA;
    LoeweEI[LoeweEI$ID==AUD$ID[i],]$ValueE<-NA;
  }
  
  LoeweEI[LoeweEI$Date<="1999-01-01"&LoeweEI$ID==1000,]$ValueI<-NA
  LoeweEI[LoeweEI$Date<="1999-01-01"&LoeweEI$ID==1000,]$ValueE<-NA
  
  LoeweEI[LoeweEI$Date>="1999-01-01"&LoeweEI$ID==1004,]$ValueI<-NA
  LoeweEI[LoeweEI$Date>="1999-01-01"&LoeweEI$ID==1004,]$ValueE<-NA
  
  LoeweEI[LoeweEI$Date>="1999-01-01"&LoeweEI$ID==1005,]$ValueI<-NA
  LoeweEI[LoeweEI$Date>="1999-01-01"&LoeweEI$ID==1005,]$ValueE<-NA
  
 LoeweEI$year<-year(LoeweEI$Date)
 
  for(i in Laender1$ID){print(i)
  t<-Comtradeorig[Comtradeorig$RepCo==i,]
    max<-max(unique(t$Year))
    min<-min(unique(t$Year))
    LoeweEI$ValueI[LoeweEI$ID==i & LoeweEI$year<min]<-NA
    LoeweEI$ValueE[LoeweEI$ID==i & LoeweEI$year<min]<-NA
    LoeweEI$ValueI[LoeweEI$ID==i & LoeweEI$year>max]<-NA
    LoeweEI$ValueE[LoeweEI$ID==i & LoeweEI$year>max]<-NA
  }
 
  LoeweEI<-LoeweEI[,(1:5)]
  
  LoeweEI<-LoeweEI[!(is.na(LoeweEI$ValueE)&is.na(LoeweEI$ValueI)==TRUE),]  #Auschlie?en wo Export UND Importe fehlen
  
  LoeweEI$ID<-factor(LoeweEI$ID)
  LoeweEI$Rep<-factor(LoeweEI$Rep)
  
  LaenderLoewe<<-data.frame(unique(LoeweEI$ID),unique(LoeweEI$Rep))
  colnames(LaenderLoewe)<<-c("ID","Rep")
 
  return(LoeweEI)
}
aug.DF<-function(dat,Variable){
  for (i in 1:3){print(i)
    for (j in 1:length(LaenderID$ID)){
      if (i==1 & j==1){Stationaritaetstest<-matrix(NA,ncol=3,nrow=length(LaenderID$ID))
      colnames(Stationaritaetstest)<-c("stationär","drift","trend")}
      RepID<-as.numeric(as.character(LaenderID$ID[j]))
      if(length(na.trim(dat[dat$ID==RepID,which(colnames(dat)==Variable)]))!=0){
        teststat<-ur.df(na.trim(dat[dat$ID==RepID,which(colnames(dat)==Variable)]),type = "trend",selectlags = "AIC")@teststat[1,i]
        kritval <-ur.df(na.trim(dat[dat$ID==RepID,which(colnames(dat)==Variable)]),type = "trend",selectlags = "AIC")@cval[i,1]
        ifelse(abs(teststat)>=abs(kritval),Stationaritaetstest[j,i]<-"Ja",Stationaritaetstest[j,i]<-"Nein")
      }else{
        Stationaritaetstest[j,i]<-"NA"
      }
    }
  }
  Stationaritaetstest<-as.data.frame(Stationaritaetstest)
  Stationaritaetstest$ID<-LaenderID$ID
  Stationaritaetstest$Rep<-LaenderID$Rep
  return(Stationaritaetstest)
}
KPSS<-function(dat,Variable){
  for (j in 1:length(LaenderID$ID)){print(j)
    if (j==1){Stationaritaetstest<-matrix(NA,ncol=1,nrow=length(LaenderID$ID))}
    RepID<-as.numeric(as.character(LaenderID$ID[j]))
    if(length(na.trim(dat[dat$ID==RepID,which(colnames(dat)==Variable)]))!=0){
      teststat<-ur.kpss(na.trim(dat[dat$ID==RepID,which(colnames(dat)==Variable)]),type="mu",lags="short")@teststat
      kritval <-ur.kpss(na.trim(dat[dat$ID==RepID,which(colnames(dat)==Variable)]),type="mu",lags="short")@cval[1,4]
      ifelse(abs(teststat)>=abs(kritval),Stationaritaetstest[j,1]<-"Ja",Stationaritaetstest[j,1]<-"Nein")
    }else{
      Stationaritaetstest[j,1]<-"NA"
    }
  }
  Stationaritaetstest<-as.data.frame(Stationaritaetstest)
  Stationaritaetstest$ID<-LaenderID$ID
  Stationaritaetstest$Rep<-LaenderID$Rep
  return(Stationaritaetstest)
}
ERS<-function(dat,Variable){
  for (j in 1:length(LaenderID$ID)){print(j)
    if (j==1){Stationaritaetstest<-matrix(NA,ncol=1,nrow=length(LaenderID$ID))}
    RepID<-as.numeric(as.character(LaenderID$ID[j]))
    if(length(na.trim(dat[dat$ID==RepID,which(colnames(dat)==Variable)]))!=0){
      teststat<-ur.ers(na.trim(dat[dat$ID==RepID,which(colnames(dat)==Variable)]), type = "DF-GLS", model = "trend",lag.max = 4)@teststat
      kritval <-ur.ers(na.trim(dat[dat$ID==RepID,which(colnames(dat)==Variable)]), type = "DF-GLS", model = "trend",lag.max = 4)@cval[1,3]
      ifelse(abs(teststat)>=abs(kritval),Stationaritaetstest[j,1]<-"Ja",Stationaritaetstest[j,1]<-"Nein")
    }else{
      Stationaritaetstest[j,1]<-"NA"
    }
  }
  Stationaritaetstest<-as.data.frame(Stationaritaetstest)
  Stationaritaetstest$ID<-LaenderID$ID
  Stationaritaetstest$Rep<-LaenderID$Rep
  return(Stationaritaetstest)
}
model<-function(Daten){forecast<-data.frame(matrix(NA,ncol=14,nrow=LaenderNum))
colnames(forecast)<-c("Rep",
                      "Row1","Rew1","Random1",
                      "RoW1.model","ReW1.model","DM.RoW1","DM.ReW1","DM.RoW.Drift","DM.ReW.Drift","DM.RoWSUP","DM.ReWSUP","SupModell")
for (j in 1:LaenderNum){print(j)  #j loop through all countries
  RepNum<-LaenderRo[j] # Number of Specific country (from Comstat)
  Namen<-LaenderNamen[j] #Name of Country j
  Data.Land<-Daten[Daten$ID==RepNum,] #Extract country data
  if(length(Data.Land$Date)>36){ #just forecast if there is enough data (>36 data points)
    forecastE1.RoW<-NA #create basic variables
    forecastE1.ReW<-NA
    forecastI1.RoW<-NA
    forecastI1.ReW<-NA
    forecastEI1.RoW<-NA
    forecastEI1.ReW<-NA
    random1<-NA
    random1_ReW<-NA
    random1_RoW<-NA
    test1<-NA
    error.forecastE1.RoW<-NA
    error.forecastE1.ReW<-NA
    error.forecastI1.RoW<-NA
    error.forecastI1.ReW<-NA
    error.forecastEI1.RoW<-NA
    error.forecastEI1.ReW<-NA
    error.random1<-NA
    error.random1_ReW<-NA
    error.random1_RoW<-NA
    
    for (i in 1:(length(Data.Land$ID)-Start.n)){ #loop through all available periods 
      trainRoW<-Data.Land[i:(Start.n-1+i),]  #Training data in Period i (RoW)
      trainReW<-Data.Land[1:(Start.n-1+i),]  #Training data in Period i (ReW)
      test<-Data.Land[(Start.n+i):length(Data.Land$ID),] #Test data in Period i
      
      regressionERoW<-lm(dWK.average.log~dValueE.log,data=trainRoW) #Train different model
      regressionIRoW<-lm(dWK.average.log~dValueI.log,data=trainRoW)
      regressionEIRoW<-lm(dWK.average.log~dValueI.log+dValueE.log,data=trainRoW)
      regressionEReW<-lm(dWK.average.log~dValueE.log,data=trainReW)
      regressionIReW<-lm(dWK.average.log~dValueI.log,data=trainReW)
      regressionEIReW<-lm(dWK.average.log~dValueI.log+dValueE.log,data=trainReW)
      
      predERoW<-predict(regressionERoW,newdata=test) #Predict next Values 
      predIRoW<-predict(regressionIRoW,newdata=test)
      predEIRoW<-predict(regressionEIRoW,newdata=test)
      predEReW<-predict(regressionEReW,newdata=test)
      predIReW<-predict(regressionIReW,newdata=test)
      predEIReW<-predict(regressionEIReW,newdata=test)
      
      forecastE1.RoW[i]<-predERoW[1] #take forecast for t+1 (could be shortend)
      forecastI1.RoW[i]<-predIRoW[1]
      forecastEI1.RoW[i]<-predEIRoW[1]
      forecastE1.ReW[i]<-predEReW[1]
      forecastI1.ReW[i]<-predIReW[1]
      forecastEI1.ReW[i]<-predEIReW[1]
      
      random1[i]<-test$dWK.average.log.lag1[1] #use random walk (without drift)
      #gleiches Ergebnis:rwf(trainReW$dWK.average.log,h=1,drift=FALSE)
      random1_ReW[i]<-c(rwf(trainReW$dWK.average.log,h=1,drift=TRUE)$mean)
      if(sum(trainRoW$dWK.average.log,na.rm=TRUE)!=0){
      random1_RoW[i]<-c(rwf(trainRoW$dWK.average.log,h=1,drift=TRUE)$mean)
      } else {
        random1_RoW[i]<-random1[i]
        }
      
      test1[i]<-test$dWK.average.log[1] #actual value
      
    }
   
    forecast[j,1]<-Namen #not needed?
    
    error.forecastE1.RoW<-forecastE1.RoW-test1 #Prediction errors
    error.forecastI1.RoW<-forecastI1.RoW-test1
    error.forecastEI1.RoW<-forecastEI1.RoW-test1
    error.forecastE1.ReW<-forecastE1.ReW-test1
    error.forecastI1.ReW<-forecastI1.ReW-test1
    error.forecastEI1.ReW<-forecastEI1.ReW-test1
    error.random1<-random1-test1 #Random error
    error.random1_ReW<-random1_ReW-test1
    error.random1_RoW<-random1_RoW-test1
    
    t<-data.frame(error.forecastE1.RoW,error.forecastI1.RoW,error.forecastEI1.RoW)
    t1<-data.frame(forecastE1.RoW,forecastI1.RoW,forecastEI1.RoW)
    n<-which.min(c(mse(forecastE1.RoW,test1),mse(forecastI1.RoW,test1),mse(forecastEI1.RoW,test1)))
    Best.RoW1.error<-t[,n]
    Best.RoW1<-t1[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.RoW1<-colnames(t[n])
    
    t<-data.frame(error.forecastE1.ReW,error.forecastI1.ReW,error.forecastEI1.ReW)
    t1<-data.frame(forecastE1.ReW,forecastI1.ReW,forecastEI1.ReW)
    n<-which.min(c(mse(forecastE1.ReW,test1),mse(forecastI1.ReW,test1),mse(forecastEI1.ReW,test1)))
    Best.ReW1.error<-t[,n]
    Best.ReW1<-t1[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.ReW1<-colnames(t[n])
    
    forecast[j,2]<-mse(Best.RoW1,test1)
    forecast[j,3]<-mse(Best.ReW1,test1)
    forecast[j,4]<-mse(random1,test1)
    forecast[j,5]<-Best.Model.RoW1
    forecast[j,6]<-Best.Model.ReW1
    forecast[j,7]<-round(dm.test(Best.RoW1.error, error.random1, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    forecast[j,8]<-round(dm.test(Best.ReW1.error, error.random1, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    forecast[j,9]<-round(dm.test(Best.RoW1.error, error.random1_RoW, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    forecast[j,10]<-round(dm.test(Best.ReW1.error, error.random1_ReW, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    if(mse(Best.RoW1,test1)<mse(Best.ReW1,test1) & forecast[j,7]<=0.05){
      forecast[j,11]<-round(dm.test(Best.RoW1.error, Best.ReW1.error, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    }
    if(mse(Best.ReW1,test1)<mse(Best.RoW1,test1) & forecast[j,8]<=0.05){
      forecast[j,12]<-round(dm.test(Best.ReW1.error, Best.RoW1.error, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    }
    
  }
}
forecast[forecast[,7]&forecast[,8]>=0.05& !is.na(forecast[,7])==TRUE,13]<-"Random"
forecast[forecast[,11]<=0.05 & !is.na(forecast[,11]),13]<-"RoW"
forecast[forecast[,12]<=0.05& !is.na(forecast[,12]),13]<-"ReW"
forecast[is.na(forecast[,13]) & !is.na(forecast[,7]),13]<-"RoW/ReW"
forecast[is.na(forecast[,13]),13]<-"No"
forecast<-data.frame(forecast)
forecast$Rep<-as.character(LaenderNamen)
forecast$number<-1:126
return(forecast)
}
model_drift<-function(Daten){forecast<-data.frame(matrix(NA,ncol=9,nrow=LaenderNum))
colnames(forecast)<-c("Rep",
                      "MSE_Row","MSE_Row_drift","MSE_Random1",
                      "RoW.model","DM.statistic.RoW","DM.p-value.RoW","DM.statistic.drift","DM.p-value.RoW.drift")
for (j in 1:LaenderNum){print(j)  #j loop through all countries
  RepNum<-LaenderRo[j] # Number of Specific country (from Comstat)
  Namen<-LaenderNamen[j] #Name of Country j
  Data.Land<-Daten[Daten$ID==RepNum,] #Extract country data
  if(length(Data.Land$Date)>36){ #just forecast if there is enough data (>36 data points)
    forecastE1.RoW<-NA #create basic variables
    forecastI1.RoW<-NA
    forecastEI1.RoW<-NA
    random1<-NA
    random1_RoW<-NA
    test1<-NA
    error.forecastE1.RoW<-NA
    error.forecastI1.RoW<-NA
    error.forecastEI1.RoW<-NA
    error.random1<-NA
    error.random1_RoW<-NA
    for (i in 1:(length(Data.Land$ID)-Start.n)){ #loop through all available periods 
      trainRoW<-Data.Land[i:(Start.n-1+i),]  #Training data in Period i (RoW)
      test<-Data.Land[(Start.n+i):length(Data.Land$ID),] #Test data in Period i
      
      regressionERoW<-lm(dWK.average.log~dValueE.log,data=trainRoW) #Train different model
      regressionIRoW<-lm(dWK.average.log~dValueI.log,data=trainRoW)
      regressionEIRoW<-lm(dWK.average.log~dValueI.log+dValueE.log,data=trainRoW)
      
      predERoW<-predict(regressionERoW,newdata=test) #Predict next Values 
      predIRoW<-predict(regressionIRoW,newdata=test)
      predEIRoW<-predict(regressionEIRoW,newdata=test)

      forecastE1.RoW[i]<-predERoW[1] #take forecast for t+1 (could be shortend)
      forecastI1.RoW[i]<-predIRoW[1]
      forecastEI1.RoW[i]<-predEIRoW[1]
      
      random1[i]<-test$dWK.average.log.lag1[1] #use random walk (without drift)
      #gleiches Ergebnis:rwf(trainReW$dWK.average.log,h=1,drift=FALSE)
      
      if(sum(trainRoW$dWK.average.log,na.rm=TRUE)!=0){
        random1_RoW[i]<-c(rwf(trainRoW$dWK.average.log,h=1,drift=TRUE)$mean)
      } else {
        random1_RoW[i]<-random1[i]
      }
      
      test1[i]<-test$dWK.average.log[1] #actual value
      
    }
    
    forecast[j,1]<-Namen #not needed?
    
    error.forecastE1.RoW<-forecastE1.RoW-test1 #Prediction errors
    error.forecastI1.RoW<-forecastI1.RoW-test1
    error.forecastEI1.RoW<-forecastEI1.RoW-test1
    error.random1<-random1-test1 #Random error
    error.random1_RoW<-random1_RoW-test1
    
    t<-data.frame(error.forecastE1.RoW,error.forecastI1.RoW,error.forecastEI1.RoW)
    t1<-data.frame(forecastE1.RoW,forecastI1.RoW,forecastEI1.RoW)
    n<-which.min(c(mse(forecastE1.RoW,test1),mse(forecastI1.RoW,test1),mse(forecastEI1.RoW,test1)))
    Best.RoW1.error<-t[,n]
    Best.RoW1<-t1[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.RoW1<-colnames(t[n])
    
    forecast[j,2]<-mse(Best.RoW1,test1)
    forecast[j,3]<-mse(random1,test1)
    forecast[j,4]<-mse(random1_RoW,test1)
    forecast[j,5]<-Best.Model.RoW1
    forecast[j,6]<-round(dm.test(Best.RoW1.error, error.random1, alternative = c("less"), h = 1,power = 2)$statistic,digits = 4)
    forecast[j,7]<-round(dm.test(Best.RoW1.error, error.random1, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    forecast[j,8]<-round(dm.test(Best.RoW1.error, error.random1_RoW, alternative = c("less"), h = 1,power = 2)$statistic,digits = 4)
    forecast[j,9]<-round(dm.test(Best.RoW1.error, error.random1_RoW, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    
  }
}
forecast$Rep<-as.character(LaenderNamen)
return(forecast)
}
model_interest<-function(Daten){forecast<-data.frame(matrix(NA,ncol=7,nrow=LaenderNum))
colnames(forecast)<-c("Rep","Model","RMSE ratio","Carry DM stat","Carry p-value",
                      "Carry + Commodity DM stat","Carry + Commodity p-value")
for (j in 1:LaenderNum){print(j)  #j loop through all countries
  RepNum<-LaenderRo[j] # Number of Specific country (from Comstat)
  Namen<-LaenderNamen[j] #Name of Country j
  Data.Land<-Daten[Daten$ID==RepNum,] #Extract country data
  if(length(Data.Land$Date)>36){ #just forecast if there is enough data (>36 data points)
    forecastCarry<-NA
    forecastE1.RoW<-NA #create basic variables
    forecastI1.RoW<-NA
    forecastEI1.RoW<-NA

    random1<-NA
   
    test1<-NA
    error.forecastcarry<-NA
    error.forecastE1.RoW<-NA
    error.forecastI1.RoW<-NA
    error.forecastEI1.RoW<-NA

    error.random1<-NA
    error.random1_RoW<-NA
    
    for (i in 1:(length(Data.Land$ID)-Start.n)){ #loop through all available periods 
      trainRoW<-Data.Land[i:(Start.n-1+i),]  #Training data in Period i (RoW)
      trainReW<-Data.Land[1:(Start.n-1+i),]  #Training data in Period i (ReW)
      test<-Data.Land[(Start.n+i):length(Data.Land$ID),] #Test data in Period i
      
      if(!is.na(sum(trainRoW$di.log))){
      
      regressionCarry<-lm(dWK.average.log~di.log,data=trainRoW)
      regressionERoW<-lm(dWK.average.log~di.log+dValueE.log,data=trainRoW) #Train different model
      regressionIRoW<-lm(dWK.average.log~di.log+dValueI.log,data=trainRoW)
      regressionEIRoW<-lm(dWK.average.log~di.log+dValueI.log+dValueE.log,data=trainRoW)
    
      predcarry<-predict(regressionCarry,newdata=test)
      predERoW<-predict(regressionERoW,newdata=test) #Predict next Values 
      predIRoW<-predict(regressionIRoW,newdata=test)
      predEIRoW<-predict(regressionEIRoW,newdata=test)
    
      forecastCarry[i]<-predcarry[1]
      forecastE1.RoW[i]<-predERoW[1] #take forecast for t+1 (could be shortend)
      forecastI1.RoW[i]<-predIRoW[1]
      forecastEI1.RoW[i]<-predEIRoW[1]
      
      
      random1[i]<-test$dWK.average.log.lag1[1] #use random walk (without drift)
      
      test1[i]<-test$dWK.average.log[1] #actual value
      }
    }
    
    forecast[j,1]<-Namen #not needed?
    error.forecastcarry<-forecastCarry-test1
    error.forecastE1.RoW<-forecastE1.RoW-test1 #Prediction errors
    error.forecastI1.RoW<-forecastI1.RoW-test1
    error.forecastEI1.RoW<-forecastEI1.RoW-test1
    
    error.random1<-random1-test1 #Random error
    
    t<-data.frame(error.forecastE1.RoW,error.forecastI1.RoW,error.forecastEI1.RoW)
    t1<-data.frame(forecastE1.RoW,forecastI1.RoW,forecastEI1.RoW)
    n<-which.min(c(mse(forecastE1.RoW,test1),mse(forecastI1.RoW,test1),mse(forecastEI1.RoW,test1)))
    Best.RoW1.error<-t[,n]
    Best.RoW1<-t1[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.RoW1<-colnames(t[n])
    ?dm.test
    
    

    forecast[j,2]<-Best.Model.RoW1
    forecast[j,3]<-mse(Best.RoW1,test1)/mse(forecastCarry,test1)
    forecast[j,4]<-round(dm.test(error.forecastcarry, error.random1, alternative = c("less"), h = 1,power = 2)$statistic,digits = 4)
    forecast[j,5]<-round(dm.test(error.forecastcarry, error.random1, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    forecast[j,6]<-round(dm.test(Best.RoW1.error, error.random1, alternative = c("less"), h = 1,power = 2)$statistic,digits = 4)
    forecast[j,7]<-round(dm.test(Best.RoW1.error, error.random1, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    # vielleicht clark west test? weil wegen nestedforecast[j,8]<-round(dm.test(Best.RoW1.error, error.forecastcarry, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    
  }
}
forecast<-data.frame(forecast)
forecast$Rep<-as.character(LaenderNamen)
return(forecast)
}
improv_plot<-function(Data,Year){
  
  t<-Data[complete.cases(Data),]
  
  t$Background[t$DM>0.1]<-">0.1"
  t$Background[t$DM<=0.1]<-"<=0.1"
  t$Background[t$DM<=0.05]<-"<=0.05"
  t$Rep <- factor(t$Rep, levels = t$Rep[order(t$imp)])
  
  ggplot(t,aes(x=t$Rep,y=t$imp))+
    labs(x="Country",y="Improvement",subtitle=Year) +
    geom_bar(aes(fill=Background),stat = "identity", width = 0.7, position = position_dodge())+
    scale_fill_manual(values=c("#55E834", "#B3FF47", "#E8BA55"),name="DM p-value")+
    geom_hline(yintercept=0)+
    theme(panel.background = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(size=.1, color="grey" ),
          legend.justification=c(1,0), legend.position=c(1,0),
          axis.text.x = element_text(colour="grey20",size=5,angle=90,face="plain"))+
    coord_flip()
}
aufbereiten<-function(Daten){
  
  f1<-Daten[,c(1:4)]
  f1$f<-1
  full<-rbind(melt(f1,id.vars=list("Rep","f")))
  full$value<-as.numeric(full$value)
  
  t<-full[full$value==aggregate(full$value[full$f==1],by=list(full$Rep[full$f==1]),min)$x,]
  full$imp<-NA
  for(i in c(1)){
    for(j in 1:126){
      country<-LaenderNamen[j]
      d<-subset(full, full$Rep==country &full$f==i)
      full$imp[full$Rep==country&full$f==i]<-100-d$value/d$value[d$variable=="Random1"]*100
    }
  }
  
  full<-full[order(full$value),]
  full$f<-factor(full$f,levels=unique(full$f))
  full<-full[full$Rep!="Maldives",]
  prog<-subset(full,full$f==1&full$variable!="Random1")
  prog<-prog[order(prog$imp),]
  prog$Rep<-factor(prog$Rep,levels=unique(prog$Rep))
  return(prog)
}
grid<-function(data){
  grid<-data
  grid$improRow<-(1-grid$Row1/grid$Random1)*100
  grid$improRew<-(1-grid$Rew1/grid$Random1)*100
  grid<-subset(grid,!is.na(grid$Random1))
  return(grid)
}
model_clarkandwest<-function(Daten){forecast<-data.frame(matrix(NA,ncol=8,nrow=LaenderNum))
colnames(forecast)<-c("Rep","Row1","Rew1","Random1","CW_RoW","DM.RoW1","CW_ReW","DM.ReW1")
for (j in 1:LaenderNum){print(j)  #j loop through all countries
  RepNum<-LaenderRo[j] # Number of Specific country (from Comstat)
  Namen<-LaenderNamen[j] #Name of Country j
  Data.Land<-Daten[Daten$ID==RepNum,] #Extract country data
  if(length(Data.Land$Date)>36){ #just forecast if there is enough data (>36 data points)
    forecastE1.RoW<-NA #create basic variables
    forecastE1.ReW<-NA
    forecastI1.RoW<-NA
    forecastI1.ReW<-NA
    forecastEI1.RoW<-NA
    forecastEI1.ReW<-NA
    random1<-NA
    random1_ReW<-NA
    random1_RoW<-NA
    test1<-NA
    error.forecastE1.RoW<-NA
    error.forecastE1.ReW<-NA
    error.forecastI1.RoW<-NA
    error.forecastI1.ReW<-NA
    error.forecastEI1.RoW<-NA
    error.forecastEI1.ReW<-NA
    error.random1<-NA
    error.random1_ReW<-NA
    error.random1_RoW<-NA
    
    for (i in 1:(length(Data.Land$ID)-Start.n)){ #loop through all available periods 
      trainRoW<-Data.Land[i:(Start.n-1+i),]  #Training data in Period i (RoW)
      trainReW<-Data.Land[1:(Start.n-1+i),]  #Training data in Period i (ReW)
      test<-Data.Land[(Start.n+i):length(Data.Land$ID),] #Test data in Period i
      
      regressionERoW<-lm(dWK.average.log~dValueE.log,data=trainRoW) #Train different model
      regressionIRoW<-lm(dWK.average.log~dValueI.log,data=trainRoW)
      regressionEIRoW<-lm(dWK.average.log~dValueI.log+dValueE.log,data=trainRoW)
      regressionEReW<-lm(dWK.average.log~dValueE.log,data=trainReW)
      regressionIReW<-lm(dWK.average.log~dValueI.log,data=trainReW)
      regressionEIReW<-lm(dWK.average.log~dValueI.log+dValueE.log,data=trainReW)
      
      predERoW<-predict(regressionERoW,newdata=test) #Predict next Values 
      predIRoW<-predict(regressionIRoW,newdata=test)
      predEIRoW<-predict(regressionEIRoW,newdata=test)
      predEReW<-predict(regressionEReW,newdata=test)
      predIReW<-predict(regressionIReW,newdata=test)
      predEIReW<-predict(regressionEIReW,newdata=test)
      
      forecastE1.RoW[i]<-predERoW[1] #take forecast for t+1 (could be shortend)
      forecastI1.RoW[i]<-predIRoW[1]
      forecastEI1.RoW[i]<-predEIRoW[1]
      forecastE1.ReW[i]<-predEReW[1]
      forecastI1.ReW[i]<-predIReW[1]
      forecastEI1.ReW[i]<-predEIReW[1]
      
      random1[i]<-test$dWK.average.log.lag1[1] #use random walk (without drift)
      #gleiches Ergebnis:rwf(trainReW$dWK.average.log,h=1,drift=FALSE)
      random1_ReW[i]<-c(rwf(trainReW$dWK.average.log,h=1,drift=TRUE)$mean)
      
      test1[i]<-test$dWK.average.log[1] #actual value
      
    }
    
    error.forecastE1.RoW<-forecastE1.RoW-test1 #Prediction errors
    error.forecastI1.RoW<-forecastI1.RoW-test1
    error.forecastEI1.RoW<-forecastEI1.RoW-test1
    error.forecastE1.ReW<-forecastE1.ReW-test1
    error.forecastI1.ReW<-forecastI1.ReW-test1
    error.forecastEI1.ReW<-forecastEI1.ReW-test1
    error.random1<-random1-test1 #Random error
    error.random1_ReW<-random1_ReW-test1
    error.random1_RoW<-random1_RoW-test1
    
    t<-data.frame(error.forecastE1.RoW,error.forecastI1.RoW,error.forecastEI1.RoW)
    t1<-data.frame(forecastE1.RoW,forecastI1.RoW,forecastEI1.RoW)
    n<-which.min(c(mse(forecastE1.RoW,test1),mse(forecastI1.RoW,test1),mse(forecastEI1.RoW,test1)))
    Best.RoW1.error<-t[,n]
    Best.RoW1<-t1[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.RoW1<-colnames(t[n])
    
    t<-data.frame(error.forecastE1.ReW,error.forecastI1.ReW,error.forecastEI1.ReW)
    t1<-data.frame(forecastE1.ReW,forecastI1.ReW,forecastEI1.ReW)
    n<-which.min(c(mse(forecastE1.ReW,test1),mse(forecastI1.ReW,test1),mse(forecastEI1.ReW,test1)))
    Best.ReW1.error<-t[,n]
    Best.ReW1<-t1[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.ReW1<-colnames(t[n])
    
    forecast[j,2]<-mse(Best.RoW1,test1)
    forecast[j,3]<-mse(Best.ReW1,test1)
    forecast[j,4]<-mse(random1,test1)
    forecast[j,5]<-Clark.West(Best.RoW1,random1,test1)[2]
    forecast[j,6]<-round(dm.test(Best.RoW1.error, error.random1, alternative = c("two.sided"), h = 1,power = 2)$p.value,digits = 4)
    forecast[j,7]<-Clark.West(Best.ReW1,random1,test1)[2]
    forecast[j,8]<-round(dm.test(Best.RoW1.error, error.random1, alternative = c("two.sided"), h = 1,power = 2)$p.value,digits = 4)
  }
}
forecast<-data.frame(forecast)
forecast$Rep<-as.character(LaenderNamen)
return(forecast)
}
Clark.West<-function(y1,y2,y){
  #H0 is that y1 <= y2 and H1 y1>y2
  e1     = (y-y1)^2;
  e2     = (y-y2)^2;
  e3     = (y1-y2)^2;
  f_hat = (e1 - e2 + e3);
  
  P<-length(f_hat)
  f_mean<-mean(f_hat)
  t.stat<-sqrt(P)*f_mean/(var(f_hat-f_mean))^0.5
  p.val<-1-pt(t.stat,df = df)
  
  result<-data.frame(t.stat,p.val)
  colnames(result)<-c("t.stat","p.val")
  return(result)
}

################

Preise1 <- read_excel("data/Price_data.xlsx",sheet = "RData",na="0")
Comtradeorig <- read_excel("data/comtrade.xlsx",na="0") 

Basis<-length(seq(as.Date("1992/12/1"),as.Date("2008/07/1"), by = "month")) #Beim 2. as.Date eintragen
startdate<-as.Date('1992-12-01 00:00:00') # Ein Monat davor und danach damit man bei der differenzierung das richtige Zeitfenster hat
Enddate<-as.Date('2017-02-01 00:00:00')

Cometradeverf<-merge(aggregate(Comtradeorig$Year,by=list(Comtradeorig$Rep),min),aggregate(Comtradeorig$Year,by=list(Comtradeorig$Rep),max),by=c("Group.1"))

#######Preismatrix#######
time<-seq(as.Date("1992/12/1"),as.Date("2016/12/1"), by = "month")

NYear<-length(time)
t<-order(Preise1$year,Preise1$Code)
Preise<-Preise1[t,];rm(Preise1)
Preise<-subset(Preise,year<=as.POSIXct('2017-01-01 00:00:00',zone="CET"))
Preise$Value<-as.numeric(Preise$Value)

Rohstoffe<-rohstoffe(Comtradeorig)
ComNames<-factor(Rohstoffe$ComNames[-40])
ComNumes<-factor(Rohstoffe$ComNum[-40])
NCom<-length(ComNames)

Preismatrix<-matrix(Preise$Value,ncol=NCom,nrow=NYear,byrow=TRUE,dimnames=list(time,ComNames))
Preismatrix<-na.approx(Preismatrix,maxgap = 3) #Approximieren von Zwischenr?umen die H?CHSTENS 3 lang sind
Preismatrix[is.na(Preismatrix)] <- 0


#####Gewichte####
xrelE<-handel(Comtradeorig,2)
xrelI<-handel(Comtradeorig,1)

xrelENetto<-handel.netto(Comtradeorig,2)
xrelINetto<-handel.netto(Comtradeorig,1)

####Kombination####

LoeweEI<-loewe(xrelE,xrelI,Preismatrix)
LoeweEInetto<-loewe(xrelENetto,xrelINetto,Preismatrix)

####Exchangerates####

WKmonthly <- read_excel("data/fx_monthly.xlsx")

WKmonthly<-subset(WKmonthly,WKmonthly$ID %in% LaenderLoewe[,1])
WKmonthly$Date<-as.Date(WKmonthly$Date)
WKmonthly$Special[is.na(WKmonthly$Special)]<-"keine"
colnames(WKmonthly)<-c("Date","WK.average","WK.min","WK.max","Workingdays","ID","Special")
WKmonthly<-subset(WKmonthly,WKmonthly$Date<as.Date("2017-01-01"))

aggregate(WKmonthly,by=list(Catergory=WKmonthly$ID),FUN= length) #Jedes Land hat 289 Beobachtungen

LaenderWK <- read_excel("data/fx_daily.xlsx",sheet = "Tabelle1")
LaenderWK<-subset(LaenderWK,LaenderWK$RepNum %in% LaenderLoewe[,1])

LaenderWK<-LaenderWK[-1]
WKdaily <- read_excel("data/fx_daily.xlsx")
WKdaily<-subset(WKdaily,WKdaily$ISO %in% LaenderWK$ISO)

setDT(LaenderWK)
setDT(WKdaily)

WKdaily <- data.frame(merge(LaenderWK, WKdaily, by = "ISO", all.y=T,allow.cartesian=TRUE))
colnames(WKdaily)<-c("ISO","Rep","ID","Bemerkung","Date","Weekday","Data")
WKdaily$Date<-as.Date(WKdaily$Date,origin="1970-01-01")
WKdaily<-aggregate(WKdaily$Data,list(format(WKdaily$Date, format="%y-%m"),WKdaily$ID),max)
colnames(WKdaily)<-c("Date","ID","WK.end")
WKdaily$Date<-as.Date(paste(WKdaily$Date,"-01",sep=""),format="%y-%m-%d")

#Merge
WK<-data.frame(merge(WKmonthly,WKdaily,by=c("Date","ID"),all.x =TRUE))
WK<-WK[!with(WK,is.na(WK.average)& is.na(WK.end)),]
WK<-subset(WK,WK$Date>=startdate & WK$Date<=Enddate)
WK$ID<-factor(WK$ID)

####Wechselkurse_JPY####

WK_JPY_single<-subset(WK,WK$ID=="392")

t<-merge(WK[,c(1,2,3,4,5,8)],WK_JPY_single[,c(1,3,4,5,8)],by="Date")
WK_JPY<-WK[,c(1,2,6,7)]
WK_JPY$WK.average<-t$WK.average.x/t$WK.average.y
WK_JPY$WK.min<-t$WK.min.x/t$WK.min.y
WK_JPY$WK.max<-t$WK.max.x/t$WK.max.y
WK_JPY$WK.end<-t$WK.end.x/t$WK.end.y

####Wechselkurse_GBP####
WK_GBP_single<-subset(WK,WK$ID=="826")

t<-merge(WK[,c(1,2,3,4,5,8)],WK_GBP_single[,c(1,3,4,5,8)],by="Date")
WK_GBP<-WK[,c(1,2,6,7)]
WK_GBP$WK.average<-t$WK.average.x/t$WK.average.y
WK_GBP$WK.min<-t$WK.min.x/t$WK.min.y
WK_GBP$WK.max<-t$WK.max.x/t$WK.max.y
WK_GBP$WK.end<-t$WK.end.x/t$WK.end.y

#####Gesamt_USD####

setDT(LoeweEI)
setDT(LoeweEInetto)

Loeweges<-data.frame(merge(LoeweEI,LoeweEInetto,by=c("Date","ID","Rep"),all.x=TRUE))

setDT(WK)
dataroh_USD<-data.frame(merge(Loeweges,WK,by=c("Date","ID"),all.x =TRUE))
dataroh_USD<-dataroh_USD[!with(dataroh_USD,is.na(WK.average)& is.na(WK.end)),]
aggregate(dataroh_USD,by=list(Catergory=dataroh_USD$ID),FUN=length)
colnames(dataroh_USD)<-c("Date","ID","Rep","ValueE","ValueI","ValueEnetto","ValueInetto","WK.average","WK.min","WK.max","Workingdays","Special","WK.end")

#Logs
logdata1_USD<-log(dataroh_USD[,which(names(dataroh_USD) %in% c("ValueE","ValueI","ValueEnetto","ValueInetto","WK.average","WK.min","WK.max","WK.end"))])
logdata_USD<-data.frame(dataroh_USD,logdata1_USD)
colnames(logdata_USD)<-c(colnames(dataroh_USD),"logValueE","logValueI","logValueEnetto","logValueInetto","WK.average.log","logMin","logMax","logWKend")

#Erste Differenzen
dValueE<-ave(logdata_USD$logValueE,factor(logdata_USD$ID),FUN=function(x) c(NA,diff(x)))
dValueI<-ave(logdata_USD$logValueI,factor(logdata_USD$ID),FUN=function(x) c(NA,diff(x)))
dValueEnetto<-ave(logdata_USD$logValueEnetto,factor(logdata_USD$ID),FUN=function(x) c(NA,diff(x)))
dValueInetto<-ave(logdata_USD$logValueInetto,factor(logdata_USD$ID),FUN=function(x) c(NA,diff(x)))
dAverage.log<-ave(logdata_USD$WK.average.log,factor(logdata_USD$ID),FUN=function(x) c(NA,diff(x)))
dMax<-ave(logdata_USD$logMax,factor(logdata_USD$ID),FUN=function(x) c(NA,diff(x)))
dMin<-ave(logdata_USD$logMin,factor(logdata_USD$ID),FUN=function(x) c(NA,diff(x)))
dWKend<-ave(logdata_USD$logWKend,factor(logdata_USD$ID),FUN=function(x) c(NA,diff(x)))
firstdiff_USD<-cbind(dValueE,dValueI,dValueEnetto,dValueInetto,dAverage.log,dMax,dMin,dWKend)
data_USD<-data.frame(logdata_USD,firstdiff_USD)
colnames(data_USD)<-c(colnames(logdata_USD),"dValueE.log","dValueI.log","dValueEnetto.log","dValueInetto.log","dWK.average.log","dMax.log","dMin.log","dWKend.log")
#Lag
data_USD<-within(data_USD, dWK.average.log.lag1 <- ave(data_USD$dWK.average.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,1)))
data_USD<-within(data_USD, dValueE.log.lag1 <- ave(data_USD$dValueE.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,1)))
data_USD<-within(data_USD, dValueI.log.lag1 <- ave(data_USD$dValueI.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,1)))
data_USD<-within(data_USD, dValueEnetto.log.lag1 <- ave(data_USD$dValueEnetto.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,1)))
data_USD<-within(data_USD, dValueInetto.log.lag1 <- ave(data_USD$dValueInetto.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,1)))
data_USD<-within(data_USD, dWK.average.log.lag3 <- ave(data_USD$dWK.average.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,3)))
data_USD<-within(data_USD, dValueE.log.lag3 <- ave(data_USD$dValueE.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,3)))
data_USD<-within(data_USD, dValueI.log.lag3 <- ave(data_USD$dValueI.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,3)))
data_USD<-within(data_USD, dValueEnetto.log.lag3 <- ave(data_USD$dValueEnetto.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,3)))
data_USD<-within(data_USD, dValueInetto.log.lag3 <- ave(data_USD$dValueInetto.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,3)))
data_USD<-within(data_USD, dWK.average.log.lag6 <- ave(data_USD$dWK.average.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,6)))
data_USD<-within(data_USD, dValueE.log.lag6 <- ave(data_USD$dValueE.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,6)))
data_USD<-within(data_USD, dValueI.log.lag6 <- ave(data_USD$dValueI.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,6)))
data_USD<-within(data_USD, dValueEnetto.log.lag6 <- ave(data_USD$dValueEnetto.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,6)))
data_USD<-within(data_USD, dValueInetto.log.lag6 <- ave(data_USD$dValueInetto.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,6)))
data_USD<-within(data_USD, dWK.average.log.lag12 <- ave(data_USD$dWK.average.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,12)))
data_USD<-within(data_USD, dValueE.log.lag12 <- ave(data_USD$dValueE.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,12)))
data_USD<-within(data_USD, dValueI.log.lag12 <- ave(data_USD$dValueI.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,12)))
data_USD<-within(data_USD, dValueEnetto.log.lag12 <- ave(data_USD$dValueEnetto.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,12)))
data_USD<-within(data_USD, dValueInetto.log.lag12 <- ave(data_USD$dValueInetto.log,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,12)))

data_USD<-within(data_USD, WK.average.lag1 <- ave(data_USD$WK.average,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,1)))
data_USD<-within(data_USD, WK.average.lag3 <- ave(data_USD$WK.average,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,3)))
data_USD<-within(data_USD, WK.average.lag6 <- ave(data_USD$WK.average,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,6)))
data_USD<-within(data_USD, WK.average.lag12 <- ave(data_USD$WK.average,factor(data_USD$ID),FUN=function(x) dplyr::lag(x,12)))

data_USD$year<-year(data_USD$Date)
data_USD$time[data_USD$year<2001]<-1#vor Dotcom
data_USD$time[data_USD$year>=2001&data_USD$year<=2007]<-"2"#Bis zur Finanzkrise
data_USD$time[data_USD$year>=2007&data_USD$year<=2011]<-"3"#Eurocrises
data_USD$time[data_USD$year>=2011]<-"4"
data_USD$time<-factor(data_USD$time,labels = (c("1993-2001","2001-2007","2007-2011","2011-2016")))

data_USD$ID<-factor(data_USD$ID)
data_USD$Rep<-factor(data_USD$Rep)

rm(data1,dataroh_USD,firstdiff_USD,logdata_USD,logdata1_USD,dAverage,dMax,dMin,dValueE,dValueI,dWKend,dValueIloglag1,dValueEloglag1,dAverage.log,dValueEnetto,dValueInetto)

LaenderID<-data.frame(unique(data_USD$Rep),unique(data_USD$ID))
colnames(LaenderID)<-c("Rep","ID")
LaenderID<-LaenderID[order(LaenderID$Rep),]

data_USD<-subset(data_USD,Date>"1992-12-01")
data_USD<-data_USD[!(data_USD$ID == "780" & data_USD$Date >"2000-07-01"), ] #Trinidad hat ab da den Wechselkurs gefixt
data_USD<-data_USD[order(data_USD$Date),]
data9303_USD<-subset(data_USD,data_USD$Date<"2004-01-01")
data0416_USD<-subset(data_USD,data_USD$Date>="2004-01-01")
pdata_USD<-pdata.frame(data_USD,index=c("Rep","Date"))

####Gesamt_JPY####
setDT(LoeweEI)
setDT(LoeweEInetto)

Loeweges<-data.frame(merge(LoeweEI,LoeweEInetto,by=c("Date","ID","Rep"),all.x=TRUE))

setDT(WK_JPY)
dataroh_JPY<-data.frame(merge(Loeweges,WK_JPY,by=c("Date","ID"),all.x =TRUE))
dataroh_JPY<-dataroh_JPY[!with(dataroh_JPY,is.na(WK.average)& is.na(WK.end)),]
aggregate(dataroh_JPY,by=list(Catergory=dataroh_JPY$ID),FUN=length)
colnames(dataroh_JPY)<-c("Date","ID","Rep","ValueE","ValueI","ValueEnetto","ValueInetto","Workingdays","Special","WK.average","WK.min","WK.max","WK.end")

#Logs
logdata1_JPY<-log(dataroh_JPY[,which(names(dataroh_JPY) %in% c("ValueE","ValueI","ValueEnetto","ValueInetto","WK.average","WK.min","WK.max","WK.end"))])
logdata_JPY<-data.frame(dataroh_JPY,logdata1_JPY)
colnames(logdata_JPY)<-c(colnames(dataroh_JPY),"logValueE","logValueI","logValueEnetto","logValueInetto","WK.average.log","logMin","logMax","logWKend")

#Erste Differenzen
dValueE<-ave(logdata_JPY$logValueE,factor(logdata_JPY$ID),FUN=function(x) c(NA,diff(x)))
dValueI<-ave(logdata_JPY$logValueI,factor(logdata_JPY$ID),FUN=function(x) c(NA,diff(x)))
dValueEnetto<-ave(logdata_JPY$logValueEnetto,factor(logdata_JPY$ID),FUN=function(x) c(NA,diff(x)))
dValueInetto<-ave(logdata_JPY$logValueInetto,factor(logdata_JPY$ID),FUN=function(x) c(NA,diff(x)))
dAverage.log<-ave(logdata_JPY$WK.average.log,factor(logdata_JPY$ID),FUN=function(x) c(NA,diff(x)))
dMax<-ave(logdata_JPY$logMax,factor(logdata_JPY$ID),FUN=function(x) c(NA,diff(x)))
dMin<-ave(logdata_JPY$logMin,factor(logdata_JPY$ID),FUN=function(x) c(NA,diff(x)))
dWKend<-ave(logdata_JPY$logWKend,factor(logdata_JPY$ID),FUN=function(x) c(NA,diff(x)))
firstdiff_JPY<-cbind(dValueE,dValueI,dValueEnetto,dValueInetto,dAverage.log,dMax,dMin,dWKend)
data_JPY<-data.frame(logdata_JPY,firstdiff_JPY)
colnames(data_JPY)<-c(colnames(logdata_JPY),"dValueE.log","dValueI.log","dValueEnetto.log","dValueInetto.log","dWK.average.log","dMax.log","dMin.log","dWKend.log")
#Lag
data_JPY<-within(data_JPY, dWK.average.log.lag1 <- ave(data_JPY$dWK.average.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,1)))
data_JPY<-within(data_JPY, dValueE.log.lag1 <- ave(data_JPY$dValueE.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,1)))
data_JPY<-within(data_JPY, dValueI.log.lag1 <- ave(data_JPY$dValueI.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,1)))
data_JPY<-within(data_JPY, dValueEnetto.log.lag1 <- ave(data_JPY$dValueEnetto.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,1)))
data_JPY<-within(data_JPY, dValueInetto.log.lag1 <- ave(data_JPY$dValueInetto.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,1)))
data_JPY<-within(data_JPY, dWK.average.log.lag3 <- ave(data_JPY$dWK.average.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,3)))
data_JPY<-within(data_JPY, dValueE.log.lag3 <- ave(data_JPY$dValueE.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,3)))
data_JPY<-within(data_JPY, dValueI.log.lag3 <- ave(data_JPY$dValueI.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,3)))
data_JPY<-within(data_JPY, dValueEnetto.log.lag3 <- ave(data_JPY$dValueEnetto.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,3)))
data_JPY<-within(data_JPY, dValueInetto.log.lag3 <- ave(data_JPY$dValueInetto.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,3)))
data_JPY<-within(data_JPY, dWK.average.log.lag6 <- ave(data_JPY$dWK.average.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,6)))
data_JPY<-within(data_JPY, dValueE.log.lag6 <- ave(data_JPY$dValueE.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,6)))
data_JPY<-within(data_JPY, dValueI.log.lag6 <- ave(data_JPY$dValueI.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,6)))
data_JPY<-within(data_JPY, dValueEnetto.log.lag6 <- ave(data_JPY$dValueEnetto.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,6)))
data_JPY<-within(data_JPY, dValueInetto.log.lag6 <- ave(data_JPY$dValueInetto.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,6)))
data_JPY<-within(data_JPY, dWK.average.log.lag12 <- ave(data_JPY$dWK.average.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,12)))
data_JPY<-within(data_JPY, dValueE.log.lag12 <- ave(data_JPY$dValueE.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,12)))
data_JPY<-within(data_JPY, dValueI.log.lag12 <- ave(data_JPY$dValueI.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,12)))
data_JPY<-within(data_JPY, dValueEnetto.log.lag12 <- ave(data_JPY$dValueEnetto.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,12)))
data_JPY<-within(data_JPY, dValueInetto.log.lag12 <- ave(data_JPY$dValueInetto.log,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,12)))

data_JPY<-within(data_JPY, WK.average.lag1 <- ave(data_JPY$WK.average,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,1)))
data_JPY<-within(data_JPY, WK.average.lag3 <- ave(data_JPY$WK.average,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,3)))
data_JPY<-within(data_JPY, WK.average.lag6 <- ave(data_JPY$WK.average,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,6)))
data_JPY<-within(data_JPY, WK.average.lag12 <- ave(data_JPY$WK.average,factor(data_JPY$ID),FUN=function(x) dplyr::lag(x,12)))

data_JPY$year<-year(data_JPY$Date)
data_JPY$time[data_JPY$year<2001]<-1#vor Dotcom
data_JPY$time[data_JPY$year>=2001&data_JPY$year<=2007]<-"2"#Bis zur Finanzkrise
data_JPY$time[data_JPY$year>=2007&data_JPY$year<=2011]<-"3"#Eurocrises
data_JPY$time[data_JPY$year>=2011]<-"4"
data_JPY$time<-factor(data_JPY$time,labels = (c("1993-2001","2001-2007","2007-2011","2011-2016")))

data_JPY$ID<-factor(data_JPY$ID)
data_JPY$Rep<-factor(data_JPY$Rep)

rm(data1,dataroh_JPY,firstdiff_JPY,logdata_JPY,logdata1_JPY,dAverage,dMax,dMin,dValueE,dValueI,dWKend,dValueIloglag1,dValueEloglag1,dAverage.log,dValueEnetto,dValueInetto)

LaenderID<-data.frame(unique(data_JPY$Rep),unique(data_JPY$ID))
colnames(LaenderID)<-c("Rep","ID")
LaenderID<-LaenderID[order(LaenderID$Rep),]

data_JPY<-subset(data_JPY,Date>"1992-12-01")
data_JPY<-subset(data_JPY,ID!="392")
data_JPY<-data_JPY[order(data_JPY$Date),]
data9303_JPY<-subset(data_JPY,data_JPY$Date<"2004-01-01")
data0416_JPY<-subset(data_JPY,data_JPY$Date>="2004-01-01")
pdata_JPY<-pdata.frame(data_JPY,index=c("Rep","Date"))

####Gesamt_GBP####

setDT(LoeweEI)
setDT(LoeweEInetto)

Loeweges<-data.frame(merge(LoeweEI,LoeweEInetto,by=c("Date","ID","Rep"),all.x=TRUE))

setDT(WK_GBP)
dataroh_GBP<-data.frame(merge(Loeweges,WK_GBP,by=c("Date","ID"),all.x =TRUE))
dataroh_GBP<-dataroh_GBP[!with(dataroh_GBP,is.na(WK.average)& is.na(WK.end)),]
aggregate(dataroh_GBP,by=list(Catergory=dataroh_GBP$ID),FUN=length)
colnames(dataroh_GBP)<-c("Date","ID","Rep","ValueE","ValueI","ValueEnetto","ValueInetto","Workingdays","Special","WK.average","WK.min","WK.max","WK.end")

#Logs
logdata1_GBP<-log(dataroh_GBP[,which(names(dataroh_GBP) %in% c("ValueE","ValueI","ValueEnetto","ValueInetto","WK.average","WK.min","WK.max","WK.end"))])
logdata_GBP<-data.frame(dataroh_GBP,logdata1_GBP)
colnames(logdata_GBP)<-c(colnames(dataroh_GBP),"logValueE","logValueI","logValueEnetto","logValueInetto","WK.average.log","logMin","logMax","logWKend")

#Erste Differenzen
dValueE<-ave(logdata_GBP$logValueE,factor(logdata_GBP$ID),FUN=function(x) c(NA,diff(x)))
dValueI<-ave(logdata_GBP$logValueI,factor(logdata_GBP$ID),FUN=function(x) c(NA,diff(x)))
dValueEnetto<-ave(logdata_GBP$logValueEnetto,factor(logdata_GBP$ID),FUN=function(x) c(NA,diff(x)))
dValueInetto<-ave(logdata_GBP$logValueInetto,factor(logdata_GBP$ID),FUN=function(x) c(NA,diff(x)))
dAverage.log<-ave(logdata_GBP$WK.average.log,factor(logdata_GBP$ID),FUN=function(x) c(NA,diff(x)))
dMax<-ave(logdata_GBP$logMax,factor(logdata_GBP$ID),FUN=function(x) c(NA,diff(x)))
dMin<-ave(logdata_GBP$logMin,factor(logdata_GBP$ID),FUN=function(x) c(NA,diff(x)))
dWKend<-ave(logdata_GBP$logWKend,factor(logdata_GBP$ID),FUN=function(x) c(NA,diff(x)))
firstdiff_GBP<-cbind(dValueE,dValueI,dValueEnetto,dValueInetto,dAverage.log,dMax,dMin,dWKend)
data_GBP<-data.frame(logdata_GBP,firstdiff_GBP)
colnames(data_GBP)<-c(colnames(logdata_GBP),"dValueE.log","dValueI.log","dValueEnetto.log","dValueInetto.log","dWK.average.log","dMax.log","dMin.log","dWKend.log")
#Lag
data_GBP<-within(data_GBP, dWK.average.log.lag1 <- ave(data_GBP$dWK.average.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,1)))
data_GBP<-within(data_GBP, dValueE.log.lag1 <- ave(data_GBP$dValueE.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,1)))
data_GBP<-within(data_GBP, dValueI.log.lag1 <- ave(data_GBP$dValueI.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,1)))
data_GBP<-within(data_GBP, dValueEnetto.log.lag1 <- ave(data_GBP$dValueEnetto.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,1)))
data_GBP<-within(data_GBP, dValueInetto.log.lag1 <- ave(data_GBP$dValueInetto.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,1)))
data_GBP<-within(data_GBP, dWK.average.log.lag3 <- ave(data_GBP$dWK.average.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,3)))
data_GBP<-within(data_GBP, dValueE.log.lag3 <- ave(data_GBP$dValueE.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,3)))
data_GBP<-within(data_GBP, dValueI.log.lag3 <- ave(data_GBP$dValueI.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,3)))
data_GBP<-within(data_GBP, dValueEnetto.log.lag3 <- ave(data_GBP$dValueEnetto.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,3)))
data_GBP<-within(data_GBP, dValueInetto.log.lag3 <- ave(data_GBP$dValueInetto.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,3)))
data_GBP<-within(data_GBP, dWK.average.log.lag6 <- ave(data_GBP$dWK.average.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,6)))
data_GBP<-within(data_GBP, dValueE.log.lag6 <- ave(data_GBP$dValueE.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,6)))
data_GBP<-within(data_GBP, dValueI.log.lag6 <- ave(data_GBP$dValueI.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,6)))
data_GBP<-within(data_GBP, dValueEnetto.log.lag6 <- ave(data_GBP$dValueEnetto.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,6)))
data_GBP<-within(data_GBP, dValueInetto.log.lag6 <- ave(data_GBP$dValueInetto.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,6)))
data_GBP<-within(data_GBP, dWK.average.log.lag12 <- ave(data_GBP$dWK.average.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,12)))
data_GBP<-within(data_GBP, dValueE.log.lag12 <- ave(data_GBP$dValueE.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,12)))
data_GBP<-within(data_GBP, dValueI.log.lag12 <- ave(data_GBP$dValueI.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,12)))
data_GBP<-within(data_GBP, dValueEnetto.log.lag12 <- ave(data_GBP$dValueEnetto.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,12)))
data_GBP<-within(data_GBP, dValueInetto.log.lag12 <- ave(data_GBP$dValueInetto.log,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,12)))

data_GBP<-within(data_GBP, WK.average.lag1 <- ave(data_GBP$WK.average,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,1)))
data_GBP<-within(data_GBP, WK.average.lag3 <- ave(data_GBP$WK.average,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,3)))
data_GBP<-within(data_GBP, WK.average.lag6 <- ave(data_GBP$WK.average,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,6)))
data_GBP<-within(data_GBP, WK.average.lag12 <- ave(data_GBP$WK.average,factor(data_GBP$ID),FUN=function(x) dplyr::lag(x,12)))

data_GBP$year<-year(data_GBP$Date)
data_GBP$time[data_GBP$year<2001]<-1#vor Dotcom
data_GBP$time[data_GBP$year>=2001&data_GBP$year<=2007]<-"2"#Bis zur Finanzkrise
data_GBP$time[data_GBP$year>=2007&data_GBP$year<=2011]<-"3"#Eurocrises
data_GBP$time[data_GBP$year>=2011]<-"4"
data_GBP$time<-factor(data_GBP$time,labels = (c("1993-2001","2001-2007","2007-2011","2011-2016")))

data_GBP$ID<-factor(data_GBP$ID)
data_GBP$Rep<-factor(data_GBP$Rep)

rm(data1,dataroh_GBP,firstdiff_GBP,logdata_GBP,logdata1_GBP,dAverage,dMax,dMin,dValueE,dValueI,dWKend,dValueIloglag1,dValueEloglag1,dAverage.log,dValueEnetto,dValueInetto)

LaenderID<-data.frame(unique(data_GBP$Rep),unique(data_GBP$ID))
colnames(LaenderID)<-c("Rep","ID")
LaenderID<-LaenderID[order(LaenderID$Rep),]

data_GBP<-subset(data_GBP,Date>"1992-12-01")
data_GBP<-subset(data_GBP,ID!="826")
data_GBP<-data_GBP[order(data_GBP$Date),]
data9303_GBP<-subset(data_GBP,data_GBP$Date<"2004-01-01")
data0416_GBP<-subset(data_GBP,data_GBP$Date>="2004-01-01")
pdata_GBP<-pdata.frame(data_GBP,index=c("Rep","Date"))

rm(a,b,Cometradeverf,Comtradeorig,dd,LaenderLoewe,LaenderWK,LoeweEI,LoeweEInetto,Loeweges,Preise,Preismatrix,Rohstoffe,t,WK,WK_GBP,WK_GBP_single,WK_JPY,WK_JPY_single,
   WKdaily,WKmonthly,xrelE,xrelENetto,xrelI,xrelI,xrelINetto,ComNames,ComNumes,NCom,NYear,rew,row,Enddate,startdate,time,x,Basis)

#####Überblick#####

#Balanced Panel
punbalancedness(data_USD,index=c("Rep","Date"))

#Korrelationsmatrix
Korrelationsmatrix<-cor(acast(data, Date ~ Rep, value.var = 'ValueE'), use = 'pairwise.complete.obs')

########Stationarität#####

LaenderID<-data.frame(unique(data_USD$Rep),unique(data_USD$ID))
colnames(LaenderID)<-c("Rep","ID")
LaenderID<-LaenderID[order(LaenderID$Rep),]

a<-aug.DF(data_USD,"WK.average")
b<-KPSS(data_USD,"WK.average")
ERS(data_USD,"WK.average")

WK.s<-merge(a,b);colnames(WK.s)<-c("ID","Rep","ADF.st","ADF.drift","ADF.trend","KPSS")
WK.s[WK.s$ADF.trend=="Nein"&WK.s$KPSS=="Ja",]#nicht Statinon?r
WK.s[WK.s$ADF.trend=="Ja"&WK.s$KPSS=="Nein",]#Statinon?r
WK.s[WK.s$ADF.trend=="Nein"&WK.s$KPSS=="Nein",]#Nicht gen?gend Beobachtungen
WK.s[WK.s$ADF.trend=="Ja"&WK.s$KPSS=="Ja",]

bvr.test(data_USD$WK.average[data_USD$Rep=="Belarus"])
bvr.test(data_USD$WK.average[data_USD$Rep=="Algeria"])
bvr.test(data_USD$WK.average[data_USD$Rep=="Mongolia"])
bvr.test(data_USD$WK.average[data_USD$Rep=="Brazil"])
bvr.test(data_USD$WK.average[data_USD$Rep=="United Kingdom"])
bvr.test(data_USD$WK.average[data_USD$Rep=="Turkey"])
bvr.test(data_USD$WK.average[data_USD$Rep=="Trinidad and Tobago"])
bvr.test(data_USD$WK.average[data_USD$Rep=="Syria"])
bvr.test(data_USD$WK.average[data_USD$Rep=="Germany"])

a<-aug.DF(data_USD,"ValueE")
b<-KPSS(data_USD,"ValueE")
ERS(data_USD,"ValueE")

E.s<-merge(a,b);colnames(E.s)<-c("ID","Rep","ADF.st","ADF.drift","ADF.trend","KPSS")
E.s[E.s$ADF.st=="Nein"&E.s$KPSS=="Ja",]#nicht Statinon?r
E.s[E.s$ADF.st=="Ja"&E.s$KPSS=="Nein",]#Statinon?r
E.s[E.s$ADF.st=="Nein"&E.s$KPSS=="Nein",]#Nicht gen?gend Beobachtungen
E.s[E.s$ADF.st=="Ja"&E.s$KPSS=="Ja",]

bvr.test(data_USD$ValueE[data$Rep=="Tonga"])

a<-aug.DF(data_USD,"ValueI")
b<-KPSS(data_USD,"ValueI")
ERS(data_USD,"ValueI")

I.s<-merge(a,b);colnames(I.s)<-c("ID","RIp","ADF.st","ADF.drift","ADF.trend","KPSS")
I.s[I.s$ADF.st=="Nein"&I.s$KPSS=="Ja",]#nicht Statinon?r
I.s[I.s$ADF.st=="Ja"&I.s$KPSS=="Nein",]#Statinon?r
I.s[I.s$ADF.st=="Nein"&I.s$KPSS=="Nein",]#Nicht gIn?gInd BIobachtungIn
I.s[I.s$ADF.st=="Ja"&I.s$KPSS=="Ja",]

aug.DF(data_USD,"WK.average.log")
KPSS(data_USD,"WK.average.log")
ERS(data_USD,"WK.average.log")
aug.DF(data_USD,"ValueE.log")
KPSS(data_USD,"ValueE.log")
ERS(data_USD,"ValueE.log")
aug.DF(data_USD,"ValueI.log")
KPSS(data_USD,"ValueI.log")
ERS(data_USD,"ValueI.log")

a<-aug.DF(data_USD,"dWK.average.log")
b<-KPSS(data_USD,"dWK.average.log")
ERS(data_USD,"dWK.average.log")

dWK.s<-merge(a,b);colnames(dWK.s)<-c("ID","Rep","ADF.st","ADF.drift","ADF.trend","KPSS")
dWK.s[dWK.s$ADF.st=="Nein"&dWK.s$KPSS=="Ja",]#nicht Statinon?r
dWK.s[dWK.s$ADF.st=="Ja"&dWK.s$KPSS=="Nein",]#Statinon?r
dWK.s[dWK.s$ADF.st=="Nein"&dWK.s$KPSS=="Nein",]#Nicht gen?gend Beobachtungen
dWK.s[dWK.s$ADF.st=="Ja"&dWK.s$KPSS=="Ja",]

bvr.test(data_USD$WK.average[data$Rep=="Ghana"])

a<-aug.DF(data_USD,"dValueE.log")
b<-KPSS(data_USD,"dValueE.log")
ERS.constant(data_USD,"dValueE.log")

dE.s<-merge(a,b);colnames(dE.s)<-c("ID","RdEp","ADF.st","ADF.drift","ADF.trdEnd","KPSS")
dE.s[dE.s$ADF.st=="Nein"&dE.s$KPSS=="Ja",]#nicht Statinon?r
dE.s[dE.s$ADF.st=="Ja"&dE.s$KPSS=="Nein",]#Statinon?r
dE.s[dE.s$ADF.st=="Nein"&dE.s$KPSS=="Nein",]#Nicht gdEn?gdEnd BdEobachtungdEn
dE.s[dE.s$ADF.st=="Ja"&dE.s$KPSS=="Ja",]

a<-aug.DF(data_USD,"dValueI.log")
b<-KPSS(data_USD,"dValueI.log")
ERS.constant(data_USD,"dValueI.log")

dI.s<-merge(a,b);colnames(dI.s)<-c("ID","RdIp","ADF.st","ADF.drift","ADF.trdInd","KPSS")
dI.s[dI.s$ADF.st=="Nein"&dI.s$KPSS=="Ja",]#nicht Statinon?r
dI.s[dI.s$ADF.st=="Ja"&dI.s$KPSS=="Nein",]#Statinon?r
dI.s[dI.s$ADF.st=="Nein"&dI.s$KPSS=="Nein",]#Nicht gdIn?gdInd BdIobachtungdIn
dI.s[dI.s$ADF.st=="Ja"&dI.s$KPSS=="Ja",]

a<-aggregate(data_USD$ValueE,by=list(data_USD$Date),mean)
b<-aggregate(data_USD$ValueI,by=list(data_USD$Date),mean)

c<-merge(a,b, by="Group.1")
colnames(c)<-c("Datum","Exportindex","Importindex")
a<-ggplot(c)+
  geom_line(aes(x=c$Datum,y=c$Exportindex,color="red"))+
  geom_line(aes(x=c$Datum,y=c$Importindex,color="dodgerblue4"))+
  labs(x="Datum",y="Indexmittel",title="Mittelwerte der Preisindizes",subtitle="?ber alle L?nder")+
  scale_color_manual(name="Modell",labels = c("Exportindex","Importindex"),values = c("Red","dodgerblue4"))
ggsave("Indexmittel.png", a)

#####seriel correlation####

data.table(data_USD)[, bgtest(dWK.average.log~dValueE.log+dValueI.log)$p.value, by=Rep] 
#Heteroskedastie
#keine Heteroskedastie
data.table(data_USD)[, bptest(dWK.average.log~dValueE.log+dValueI.log), by=Rep] 

#####Multikolinearit?t####
for(i in LaenderID$ID){
  t<-data_USD[data_USD$ID==i,]
  print(as.character(LaenderID$Rep[LaenderID$ID==i]))
  print(cor(t$dValueE.log,t$dValueI.log,use="complete.obs"))
}

xEmelt<-melt(xrelE)
xImelt<-melt(xrelI)
xEmeltn<-melt(xrelENetto)
xImeltn<-melt(xrelINetto)
Preismatrixmelt<-melt(Preismatrix)
colnames(xEmelt)<-c("Rohstoff","Land","Value")
colnames(xImelt)<-c("Rohstoff","Land","Value")
colnames(xEmeltn)<-c("Rohstoff","Land","Value")
colnames(xImeltn)<-c("Rohstoff","Land","Value")
colnames(Preismatrixmelt)<-c("Date","Rohstoff","Value")
Preismatrixmelt$Date<-as.Date(Preismatrixmelt$Date)

setdiff(unique(xImeltn$Land),unique(xImelt$Land))
t<-data.frame(xEmelt$Rohstoff[xEmelt$Land=="Turkey"],xEmelt$Value[xEmelt$Land=="Turkey"],xImelt$Value[xImelt$Land=="Turkey"])


plot(data$ValueE[data$Rep=="Turkey"],type="l")
plot(data$ValueI[data$Rep=="Turkey"])
par(new=TRUE)
plot(Preismatrixmelt$Value[Preismatrixmelt$Rohstoffe=="Petroleum oils, crude\r\n"],type="l")

t<-merge(Preismatrixmelt,xImelt[xImelt$Land=="Turkey",],by="Rohstoff")
colnames(t)<-c("Rohstoff","Date","Preis","Land","Gewicht")
t$Produkt<-t$Preis*t$Gewicht
indexE<-aggregate(t$Produkt,by=list(t$Date),sum)
indexE$x<-indexE$x/indexE$x[indexE$Group.1=="2008-07-01"]

rep(xEmelt$Value[xEmelt$Land=="Turkey"],times=289)*Preismatrixmelt$Value

cor(indexE$x,data$ValueI[data$Rep=="Turkey"])

######Einzelne Regressionen#####

lmValueE<-data.frame(matrix(NA,ncol=5))
colnames(lmValueE)<-c("Estimate","Std. Error","t value","Pr(>|t|)","sign")
for(i in LaenderID$Rep[-c(36)]){print(i);
  summary<-summary(lm(dWK.average.log~dValueE.log,data=data_USD[data_USD$Rep==i,]))
  lmValueE[i,]<-c(summary$coefficients[-1,],ifelse(summary$coefficients[-1,4]<0.01,"***",ifelse(summary$coefficients[-1,4]<0.05,"**",ifelse(summary$coefficients[-1,4]<0.1,"*","")))) 
}
lmValueE.Handel<-data.frame(lmValueE[-1,],LaenderID$Rep[-39])
colnames(lmValueE.Handel)<-c("Estimate","Std. Error","t value","Pr(>|t|)","sign","Rep")
lmValueE.Handel<-merge(lmValueE.Handel,Handelsanteile,by="Rep")

boxplot(lmValueE.Handel$Anteil.Export~lmValueE.Handel$sign)

lmValueI<-data.frame(matrix(NA,ncol=5))
colnames(lmValueI)<-c("Estimate","Std. Error","t value","Pr(>|t|)","sign")
for(i in LaenderID$Rep[-c(59)]){print(i);
  summary<-summary(lm(dWK.average.log~dValueI.log,data=data_USD[data_USD$Rep==i,]))
  lmValueI[i,]<-c(summary$coefficients[-1,],ifelse(summary$coefficients[-1,4]<0.01,"***",ifelse(summary$coefficients[-1,4]<0.05,"**",ifelse(summary$coefficients[-1,4]<0.1,"*","")))) 
}

for(i in LaenderID$Rep){ print(i)
  print(summary(lm(dWK.average.log~dValueE.log+dValueI.log,data=data_USD[data_USD$Rep==i,])))
  invisible(readline(prompt="Press [enter] to continue"))
}

lmValueI.Handel<-data.frame(lmValueI[-1,],LaenderID$Rep[-39])
colnames(lmValueI.Handel)<-c("Estimate","Std. Error","t value","Pr(>|t|)","sign","Rep")
lmValueI.Handel<-merge(lmValueI.Handel,Handelsanteile,by="Rep")

t<-lmValueI.Handel[complete.cases(lmValueI.Handel),]
boxplot(t$Anteil.Import~t$sign)
plot(t$Estimate,t$Anteil.Import)


#####Paneldata####
pdata<-pdata.frame(data_USD,c("Rep","Date"))
pdim(pdata)
punbalancedness(pdata,index="Rep")

#Volle Daten
pool.E<-plm(dWK.average.log~dValueE.log+time,pdata,model="pool");summary(pool.E)
pool.I<-plm(dWK.average.log~dValueI.log+time,pdata,model="pool");summary(pool.I)
pool<-plm(dWK.average.log~dValueE.log+dValueI.log+time,pdata,model="pool");summary(pool)
fixed.E<-plm(dWK.average.log~dValueE.log+time,pdata,model="within");summary(fixed.E)
fixed.I<-plm(dWK.average.log~dValueI.log+time,pdata,model="within");summary(fixed.I)
fixed<-plm(dWK.average.log~dValueE.log+dValueI.log+time,pdata,model="within");summary(fixed)
random.E<-plm(dWK.average.log~dValueE.log+time,pdata,model="random");summary(random.E)
random.I<-plm(dWK.average.log~dValueI.log+time,pdata,model="random");summary(random.I)
random<-plm(dWK.average.log~dValueE.log+dValueI.log+time,pdata,model="random");summary(random)
rrandom.E<-coeftest(random.E, vcov. = vcovSCC)
rrandom.I<-coeftest(random.I, vcov. = vcovSCC)
rrandom<-coeftest(random, vcov. = vcovSCC)

stargazer(random.E,random.I,random, type="html", dep.var.labels=c("Wechselkurs"), 
          covariate.labels=c("PEX","PIM","2001-2007","2007-2011","2011-2016","Konstante"), 
          out="models.htm")
stargazer(rrandom.E,rrandom.I,rrandom, type="html", dep.var.labels=c("Wechselkurs"), 
          covariate.labels=c("PEX","PIM","2001-2007","2007-2011","2011-2016","Konstante"), 
          out="models.htm")

#Data Paper
#2004-2016 Rohstoffl?nder
data.paper<-subset(data_USD,data_USD$Rep %in% c("AUD","Canada","Norway","Brazil","Chile","Colombia","Mexico","Peru","CMA","Russian Federation","Malaysia"))
data.paper<-subset(data.paper,data.paper$Date>"2003-12-1")
pdata.paper<-pdata.frame(data.paper,c("Rep","Date"))
random.E.0416.Com<-plm(dWK.average.log~dValueE.log+time,pdata.paper,model="random");summary(random.E.0416.Com)
random.I.0416.Com<-plm(dWK.average.log~dValueI.log+time,pdata.paper,model="random");summary(random.I.0416.Com)
random.EI.0416.Com<-plm(dWK.average.log~dValueE.log+dValueI.log+time,pdata.paper,model="random");summary(random.EI.0416.Com)
robust.E.0416.Com<-sqrt(diag(vcovSCC(random.E.0416.Com)))
robust.I.0416.Com<-sqrt(diag(vcovSCC(random.I.0416.Com)))
robust.EI.0416.Com<-sqrt(diag(vcovSCC(random.EI.0416.Com)))
#2004-2016 Nichtrohstoffl?nder
data.paper<-subset(data_USD,data_USD$Date>"2003-12-1")
data.paper<-subset(data.paper,!(data_USD$Rep %in% c("AUD","Canada","Norway","Brazil","Chile","Colombia","Mexico","Peru","CMA","Russian Federation","Malaysia")))
pdata.paper<-pdata.frame(data.paper,c("Rep","Date"))
random.E.0416.All<-plm(dWK.average.log~dValueE.log+time,pdata.paper,model="random");summary(random.E.0416.All)
random.I.0416.All<-plm(dWK.average.log~dValueI.log+time,pdata.paper,model="random");summary(random.I.0416.All)
random.EI.0416.All<-plm(dWK.average.log~dValueE.log+dValueI.log+time,pdata.paper,model="random");summary(random.EI.0416.All)
robust.E.0416.All<-sqrt(diag(vcovSCC(random.E.0416.All)))
robust.I.0416.All<-sqrt(diag(vcovSCC(random.I.0416.All)))
robust.EI.0416.All<-sqrt(diag(vcovSCC(random.EI.0416.All)))
#1993-2003 Rohstoffl?nder
data.paper<-subset(data_USD,data_USD$Rep %in% c("AUD","Canada","Norway","Brazil","Chile","Colombia","Mexico","Peru","CMA","Russian Federation","Malaysia"))
data.paper<-subset(data.paper,data.paper$Date<"2003-12-1")
pdata.paper<-pdata.frame(data.paper,c("Rep","Date"))
random.E.9303.Com<-plm(dWK.average.log~dValueE.log+time,pdata.paper,model="random");summary(random.E.9303.Com)
random.I.9303.Com<-plm(dWK.average.log~dValueI.log+time,pdata.paper,model="random");summary(random.I.9303.Com)
random.EI.9303.Com<-plm(dWK.average.log~dValueE.log+dValueI.log+time,pdata.paper,model="random");summary(random.EI.9303.Com)
robust.E.9303.Com<-sqrt(diag(vcovSCC(random.E.9303.Com)))
robust.I.9303.Com<-sqrt(diag(vcovSCC(random.I.9303.Com)))
robust.EI.9303.Com<-sqrt(diag(vcovSCC(random.EI.9303.Com)))
#1993-2003 Nicht Rohstoffl?nder
data.paper<-subset(data_USD,!(data_USD$Rep %in% c("AUD","Canada","Norway","Brazil","Chile","Colombia","Mexico","Peru","CMA","Russian Federation","Malaysia")))
data.paper<-subset(data.paper,data.paper$Date<"2003-12-1")
pdata.paper<-pdata.frame(data.paper,c("Rep","Date"))
random.E.9303.All<-plm(dWK.average.log~dValueE.log+time,pdata.paper,model="random");summary(random.E.9303.All)
random.I.9303.All<-plm(dWK.average.log~dValueI.log+time,pdata.paper,model="random");summary(random.I.9303.All)
random.EI.9303.All<-plm(dWK.average.log~dValueE.log+dValueI.log+time,pdata.paper,model="random");summary(random.EI.9303.All)
robust.E.9303.All<-sqrt(diag(vcovSCC(random.E.9303.All)))
robust.I.9303.All<-sqrt(diag(vcovSCC(random.I.9303.All)))
robust.EI.9303.All<-sqrt(diag(vcovSCC(random.EI.9303.All)))
#1993-2016 Rohstoffl?nder
data.paper<-subset(data_USD,data_USD$Rep %in% c("AUD","Canada","Norway","Brazil","Chile","Colombia","Mexico","Peru","CMA","Russian Federation","Malaysia"))
pdata.paper<-pdata.frame(data.paper,c("Rep","Date"))
random.E.9316.Com<-plm(dWK.average.log~dValueE.log+time,pdata.paper,model="random");summary(random.E.9316.Com)
random.I.9316.Com<-plm(dWK.average.log~dValueI.log+time,pdata.paper,model="random");summary(random.I.9316.Com)
random.EI.9316.Com<-plm(dWK.average.log~dValueE.log+dValueI.log+time,pdata.paper,model="random");summary(random.EI.9316.Com)
robust.E.9316.Com<-sqrt(diag(vcovSCC(random.E.9316.Com)))
robust.I.9316.Com<-sqrt(diag(vcovSCC(random.I.9316.Com)))
robust.EI.9316.Com<-sqrt(diag(vcovSCC(random.EI.9316.Com)))
#1993-2016 Nichtrohstoffl?nder
data.paper<-subset(data_USD,!(data_USD$Rep %in% c("AUD","Canada","Norway","Brazil","Chile","Colombia","Mexico","Peru","CMA","Russian Federation","Malaysia")))
pdata.paper<-pdata.frame(data.paper,c("Rep","Date"))
random.E.9316.All<-plm(dWK.average.log~dValueE.log+time,pdata.paper,model="random");summary(random.E.9316.All)
random.I.9316.All<-plm(dWK.average.log~dValueI.log+time,pdata.paper,model="random");summary(random.I.9316.All)
random.EI.9316.All<-plm(dWK.average.log~dValueE.log+dValueI.log+time,pdata.paper,model="random");summary(random.EI.9316.All)
robust.E.9316.All<-sqrt(diag(vcovSCC(random.E.9316.All)))
robust.I.9316.All<-sqrt(diag(vcovSCC(random.I.9316.All)))
robust.EI.9316.All<-sqrt(diag(vcovSCC(random.EI.9316.All)))

stargazer(random.E.9303.Com,random.I.9303.Com,random.EI.9303.Com,
          random.E.0416.Com,random.I.0416.Com,random.EI.0416.Com,
          random.E.9316.Com,random.I.9316.Com,random.EI.9316.Com,
          random.E.9303.All,random.I.9303.All,random.EI.9303.All,
          random.E.0416.All,random.I.0416.All,random.EI.0416.All,
          random.E.9316.All,random.I.9316.All,random.EI.9316.All,
          se  = list(
                     robust.E.9303.Com,robust.I.9303.Com,robust.EI.9303.Com,
                     robust.E.0416.Com,robust.I.0416.Com,robust.EI.0416.Com,
                     robust.E.9316.Com,robust.I.9316.Com,robust.EI.9316.Com,
                     robust.E.9303.All,robust.I.9303.All,robust.EI.9303.All,
                     robust.E.0416.All,robust.I.0416.All,robust.EI.0416.All,
                     robust.E.9316.All,robust.I.9316.All,robust.EI.9316.All),
          type="html", dep.var.labels=c("Wechselkurs"),
          covariate.labels=c("PEX","PIM","2001-2007","2007-2011","2011-2016","Constant"),
          column.labels=c("Commodity Countries","Non-Commodity Countries"),
          column.separate = c(9, 9),
          out="models.Länderrobust.htm",
          omit.stat = c("rsq"))

stargazer(random.E.0416.Com,random.E.9303.Com, 
          se  = list(
            robust.E.0416.Com,robust.E.9303.Com),
           dep.var.labels=c("Exchange Rate"),
          covariate.labels=c("PEX","2007-2011","2011-2016","2001-2007","Constant"),
          omit.stat = c("rsq"))

stargazer(random.E.0416.Com, random.I.0416.Com,random.EI.0416.Com,
          se  = list(
            robust.E.0416.Com,robust.I.0416.Com,robust.EI.0416.Com),
          dep.var.labels=c("Exchange Rate"),
          covariate.labels=c("PEX","PIM","2007-2011","2011-2016","Constant"),
          omit.stat = c("rsq"))

stargazer(random.E.0416.Com, random.E.0416.All,
          se  = list(
            robust.E.0416.Com,robust.E.0416.All),
          dep.var.labels=c("Exchange Rate"),
          covariate.labels=c("PEX","2001-2007","2007-2011","2011-2016","Constant"),
          omit.stat = c("rsq"))

stargazer(random.E.0416.Com,random.E.9303.Com,random.I.0416.Com,random.EI.0416.Com,random.E.0416.All,random.EI.0416.All,
          se  = list(
            robust.E.0416.Com,robust.E.9303.Com,robust.I.0416.Com,robust.EI.0416.Com,robust.E.0416.All,robust.EI.0416.All),
          dep.var.labels=c("Exchange Rate"),
          covariate.labels=c("PEX","PIM","2001-2003","2007-2011","2011-2016","Constant"),
          omit.stat = c("rsq"))


#2004-2016 ohne Rohstoffl?nder
data.paper<-subset(data_USD,!(data_USD$Rep %in% c("AUD","Canada","Norway","Brazil","Chile","Colombia","Mexico","Peru","CMA","Russian Federation","Malaysia")))
data.paper<-subset(data.paper,data.paper$Date>"2003-12-1")
pdata.paper<-pdata.frame(data.paper,c("Rep","Date"))
random.E.0416.nCom<-plm(dWK.average.log~dValueE.log+time,pdata.paper,model="random");summary(random.E.0416.nCom)
random.I.0416.nCom<-plm(dWK.average.log~dValueI.log+time,pdata.paper,model="random");summary(random.I.0416.nCom)
random.EI.0416.nCom<-plm(dWK.average.log~dValueE.log+dValueI.log+time,pdata.paper,model="random");summary(random.EI.0416.nCom)
robust.E.0416.nCom<-sqrt(diag(vcovSCC(random.E.0416.Com)))
robust.I.0416.nCom<-sqrt(diag(vcovSCC(random.I.0416.Com)))
robust.EI.0416.Com<-sqrt(diag(vcovSCC(random.EI.0416.Com)))


#Tests
pooltest(dWK.average.log~dValueI.log+factor(year),data=pdata,model="within")
pooltest(dWK.average.log~dValueE.log+factor(year),data=pdata,model="within")
pooltest(dWK.average.log~dValueE.log+dValueI.log+factor(year),data=pdata,model="within")

pFtest(fixed.E,pool.E)  #H0= OLS better than fixed
pFtest(fixed.I,pool.I)
pFtest(fixed,pool)

plmtest(pool.E, type=c("bp")) #BP ablehen ->Random Effects
plmtest(pool.I, type=c("bp"))
plmtest(pool, type=c("bp"))

phtest(pool.E, fixed.E, vcov = vcovSCC)
phtest(fixed.E, random.E, vcov = vcovSCC)
phtest(pool.I, fixed.I, vcov = vcovSCC)
phtest(fixed.I, random.I, vcov = vcovSCC)
phtest(pool, fixed, vcov = vcovSCC)
phtest(fixed, random, vcov = vcovSCC)

pwtest(dWK.average.log~dValueE.log+factor(year),data=pdata) #Woolridge test h0=es gibt keine nicht beobachteten Effekte in den Residuen
pwtest(dWK.average.log~dValueI.log+factor(year),data=pdata)
pwtest(dWK.average.log~dValueE.log+dValueI.log+factor(year),data=pdata)

pbgtest(pool)#seriel correlation
pbgtest(fixed)#seriel correlation
pbgtest(random)#seriel correlation

bptest(dWK.average.log~dValueE.log+dValueI.log, data = pdata, studentize=T) #Heteroskedastie

pbsytest(dWK.average.log~dValueE.log+dValueI.log+factor(year),data=pdata)
phtest(dWK.average.log~dValueE.log+dValueI.log+factor(year),data=pdata,model=c("random","pooling")) #unter 0,05 ->pool (H0: random effects sind inconstitent -> abgelehnt)

pwfdtest(WK.average.log~logValueE+logValueI+factor(year),data=pdata)

###predictive reg, insample####

random.lag1<-plm(dWK.average.log~dValueE.log.lag1+dValueI.log.lag1+pdata$time,data=pdata,model="random");summary(random.lag1)
random.lag3<-plm(dWK.average.log~dValueE.log.lag3+dValueI.log.lag3+time,data=pdata,model="random");summary(random.lag3)
random.lag6<-plm(dWK.average.log~dValueE.log.lag6+dValueI.log.lag6+time,data=pdata,model="random");summary(random.lag6)
random.lag12<-plm(dWK.average.log~dValueE.log.lag12+dValueI.log.lag12+time,data=pdata,model="random");summary(random.lag12)

stargazer(random.lag1,random.lag3,random.lag6,random.lag12, type="html", dep.var.labels=c("Wechselkurs"), 
          covariate.labels=c("PEX1","PIM1","PEX3","PIM3","PEX6","PIM6","PEX12","PIM12","2001-2007","2007-2011","2011-2016","Konstante"),
          column.labels = c("k=1", "k=3","k=6","k=12"),
          out="random.lag.htm")

random.lag1r<-coeftest(random.lag1, vcov. = vcovSCC)
random.lag3r<-coeftest(random.lag3, vcov. = vcovSCC)
random.lag6r<-coeftest(random.lag6, vcov. = vcovSCC)
random.lag12r<-coeftest(random.lag12, vcov. = vcovSCC)

stargazer(random.lag1r,random.lag3r,random.lag6r,random.lag12r, type="html", dep.var.labels=c("Wechselkurs"), 
          covariate.labels=c("PEX1","PIM1","PEX3","PIM3","PEX6","PIM6","PEX12","PIM12","2001-2007","2007-2011","2011-2016","Konstante"),
          column.labels = c("k=1", "k=3","k=6","k=12"),
          out="random.lagr.htm")

####Grundmodell#####

#RollingWindow mit X WErten aus t
#Wenn man die zuk?nftigen Rohstoffdaten kennt, dann kann man den ?lpreis vorhersagen

Start.n<-37 #Perioden f?r das erste Modell hier 36 Monate
LaenderRo<-unique(LaenderID$ID)
LaenderNamen<-unique(LaenderID$Rep)
LaenderNum<-length(LaenderRo)

#alles kombiniert
MSE.forecast<-data.frame(matrix(NA,ncol=37,nrow=LaenderNum))
colnames(MSE.forecast)<-c("Rep",
                          "Row1","Rew1","Random1","RoW1.model","ReW1.model","DM.RoW1","DM.ReW1","Vergleich1","SupModel",
                          "Row3","Rew3","Random3","RoW3.model","ReW3.model","DM.RoW3","DM.ReW3","Vergleich3","SupModel",
                          "Row6","Rew6","Random6","RoW6.model","ReW6.model","DM.RoW6","DM.ReW6","Vergleich6","SupModel",
                          "Row12","Rew12","Random12","RoW12.model","ReW12.model","DM.RoW12","DM.ReW12","Vergleich12","SupModel"
                          )
for (j in 1:LaenderNum){print(j)  #rolling window
  RepNum<-LaenderRo[j] 
  Namen<-LaenderNamen[j]
  Data.Land<-data_USD[data_USD$ID==RepNum,]
  if(length(Data.Land$Date)>36){
    forecastE1.RoW<-NA
    forecastE3.RoW<-NA
    forecastE6.RoW<-NA
    forecastE12.RoW<-NA
    forecastE1.ReW<-NA
    forecastE3.ReW<-NA
    forecastE6.ReW<-NA
    forecastE12.ReW<-NA
    forecastI1.RoW<-NA
    forecastI3.RoW<-NA
    forecastI6.RoW<-NA
    forecastI12.RoW<-NA
    forecastI1.ReW<-NA
    forecastI3.ReW<-NA
    forecastI6.ReW<-NA
    forecastI12.ReW<-NA
    forecastEI1.RoW<-NA
    forecastEI3.RoW<-NA
    forecastEI6.RoW<-NA
    forecastEI12.RoW<-NA
    forecastEI1.ReW<-NA
    forecastEI3.ReW<-NA
    forecastEI6.ReW<-NA
    forecastEI12.ReW<-NA
    random1<-NA
    random3<-NA
    random6<-NA
    random12<-NA
    test1<-NA
    test3<-NA
    test6<-NA
    test12<-NA
    error.forecastE1.RoW<-NA
    error.forecastE3.RoW<-NA
    error.forecastE6.RoW<-NA
    error.forecastE12.RoW<-NA
    error.forecastE1.ReW<-NA
    error.forecastE3.ReW<-NA
    error.forecastE6.ReW<-NA
    error.forecastE12.ReW<-NA
    error.forecastI1.RoW<-NA
    error.forecastI3.RoW<-NA
    error.forecastI6.RoW<-NA
    error.forecastI12.RoW<-NA
    error.forecastI1.ReW<-NA
    error.forecastI3.ReW<-NA
    error.forecastI6.ReW<-NA
    error.forecastI12.ReW<-NA
    error.forecastEI1.RoW<-NA
    error.forecastEI3.RoW<-NA
    error.forecastEI6.RoW<-NA
    error.forecastEI12.RoW<-NA
    error.forecastEI1.ReW<-NA
    error.forecastEI3.ReW<-NA
    error.forecastEI6.ReW<-NA
    error.forecastEI12.ReW<-NA
    error.random1<-NA
    error.random3<-NA
    error.random6<-NA
    error.random12<-NA
    scomb<-0
    ecomb<-0
    for (i in 1:(length(Data.Land$ID)-Start.n)){
      trainRoW<-Data.Land[i:(Start.n-1+i),]  
      trainReW<-Data.Land[1:(Start.n-1+i),]  
      test<-Data.Land[(Start.n+i):length(Data.Land$ID),]
      
      regressionERoW<-lm(dWK.average.log~dValueE.log,data=trainRoW)
      regressionIRoW<-lm(dWK.average.log~dValueI.log,data=trainRoW)
      regressionEIRoW<-lm(dWK.average.log~dValueI.log+dValueE.log,data=trainRoW)
      regressionEReW<-lm(dWK.average.log~dValueE.log,data=trainReW)
      regressionIReW<-lm(dWK.average.log~dValueI.log,data=trainReW)
      regressionEIReW<-lm(dWK.average.log~dValueI.log+dValueE.log,data=trainReW)
      
      predERoW<-predict(regressionERoW,newdata=test)
      predIRoW<-predict(regressionIRoW,newdata=test)
      predEIRoW<-predict(regressionEIRoW,newdata=test)
      predEReW<-predict(regressionEReW,newdata=test)
      predIReW<-predict(regressionIReW,newdata=test)
      predEIReW<-predict(regressionEIReW,newdata=test)
      
      forecastE1.RoW[i]<-predERoW[1]
      forecastE3.RoW[i]<-predERoW[3]
      forecastE6.RoW[i]<-predERoW[6]
      forecastE12.RoW[i]<-predERoW[12]
      forecastI1.RoW[i]<-predIRoW[1]
      forecastI3.RoW[i]<-predIRoW[3]
      forecastI6.RoW[i]<-predIRoW[6]
      forecastI12.RoW[i]<-predIRoW[12]
      forecastEI1.RoW[i]<-predEIRoW[1]
      forecastEI3.RoW[i]<-predEIRoW[3]
      forecastEI6.RoW[i]<-predEIRoW[6]
      forecastEI12.RoW[i]<-predEIRoW[12]
      forecastE1.ReW[i]<-predEReW[1]
      forecastE3.ReW[i]<-predEReW[3]
      forecastE6.ReW[i]<-predEReW[6]
      forecastE12.ReW[i]<-predEReW[12]
      forecastI1.ReW[i]<-predIReW[1]
      forecastI3.ReW[i]<-predIReW[3]
      forecastI6.ReW[i]<-predIReW[6]
      forecastI12.ReW[i]<-predIReW[12]
      forecastEI1.ReW[i]<-predEIReW[1]
      forecastEI3.ReW[i]<-predEIReW[3]
      forecastEI6.ReW[i]<-predEIReW[6]
      forecastEI12.ReW[i]<-predEIReW[12]
      
      random1[i]<-test$dWK.average.log.lag1[1]
      random3[i]<-test$dWK.average.log.lag3[1]
      random6[i]<-test$dWK.average.log.lag6[1]
      random12[i]<-test$dWK.average.log.lag12[1]
      
      test1[i]<-test$dWK.average.log[1]
      test3[i]<-test$dWK.average.log[3]
      test6[i]<-test$dWK.average.log[6]
      test12[i]<-test$dWK.average.log[12]
      
    }
    
    MSE.forecast[j,1]<-Namen
    
    error.forecastE1.RoW<-forecastE1.RoW-test1
    error.forecastE3.RoW<-forecastE3.RoW-test3
    error.forecastE6.RoW<-forecastE6.RoW-test6
    error.forecastE12.RoW<-forecastE12.RoW-test12
    error.forecastI1.RoW<-forecastI1.RoW-test1
    error.forecastI3.RoW<-forecastI3.RoW-test3
    error.forecastI6.RoW<-forecastI6.RoW-test6
    error.forecastI12.RoW<-forecastI12.RoW-test12
    error.forecastEI1.RoW<-forecastEI1.RoW-test1
    error.forecastEI3.RoW<-forecastEI3.RoW-test3
    error.forecastEI6.RoW<-forecastEI6.RoW-test6
    error.forecastEI12.RoW<-forecastEI12.RoW-test12
    error.forecastE1.ReW<-forecastE1.ReW-test1
    error.forecastE3.ReW<-forecastE3.ReW-test3
    error.forecastE6.ReW<-forecastE6.ReW-test6
    error.forecastE12.ReW<-forecastE12.ReW-test12
    error.forecastI1.ReW<-forecastI1.ReW-test1
    error.forecastI3.ReW<-forecastI3.ReW-test3
    error.forecastI6.ReW<-forecastI6.ReW-test6
    error.forecastI12.ReW<-forecastI12.ReW-test12
    error.forecastEI1.ReW<-forecastEI1.ReW-test1
    error.forecastEI3.ReW<-forecastEI3.ReW-test3
    error.forecastEI6.ReW<-forecastEI6.ReW-test6
    error.forecastEI12.ReW<-forecastEI12.ReW-test12
    
    error.random1<-random1-test1
    error.random3<-random3-test3
    error.random6<-random6-test6
    error.random12<-random12-test12
    
    t<-data.frame(error.forecastE1.RoW,error.forecastI1.RoW,error.forecastEI1.RoW)
    t1<-data.frame(forecastE1.RoW,forecastI1.RoW,forecastEI1.RoW)
    n<-which.min(c(mse(forecastE1.RoW,test1),mse(forecastI1.RoW,test1),mse(forecastEI1.RoW,test1)))
    Best.RoW1.error<-t[,n]
    Best.RoW1<-t1[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.RoW1<-colnames(t[n])
    
    t<-data.frame(error.forecastE3.RoW,error.forecastI3.RoW,error.forecastEI3.RoW)
    t1<-data.frame(forecastE3.RoW,forecastI3.RoW,forecastEI3.RoW)
    n<-which.min(c(mse(forecastE3.RoW,test3),mse(forecastI3.RoW,test3),mse(forecastEI3.RoW,test3)))
    Best.RoW3.error<-t[,n]
    Best.RoW3<-t1[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.RoW3<-colnames(t[n])
    
    t<-data.frame(error.forecastE6.RoW,error.forecastI6.RoW,error.forecastEI6.RoW)
    t1<-data.frame(forecastE6.RoW,forecastI6.RoW,forecastEI6.RoW)
    n<-which.min(c(mse(forecastE6.RoW,test6),mse(forecastI6.RoW,test6),mse(forecastEI6.RoW,test6)))
    Best.RoW6.error<-t[,n]
    Best.RoW6<-t1[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.RoW6<-colnames(t[n])
    
    t<-data.frame(error.forecastE12.RoW,error.forecastI12.RoW,error.forecastEI12.RoW)
    t1<-data.frame(forecastE12.RoW,forecastI12.RoW,forecastEI12.RoW)
    n<-which.min(c(mse(forecastE12.RoW,test12),mse(forecastI12.RoW,test12),mse(forecastEI12.RoW,test12)))
    Best.RoW12.error<-t[,n]
    Best.RoW12<-t1[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.RoW12<-colnames(t[n])
    
    t<-data.frame(error.forecastE1.ReW,error.forecastI1.ReW,error.forecastEI1.ReW)
    t1<-data.frame(forecastE1.ReW,forecastI1.ReW,forecastEI1.ReW)
    n<-which.min(c(mse(forecastE1.ReW,test1),mse(forecastI1.ReW,test1),mse(forecastEI1.ReW,test1)))
    Best.ReW1.error<-t[,n]
    Best.ReW1<-t1[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.ReW1<-colnames(t[n])
    
    t<-data.frame(error.forecastE3.ReW,error.forecastI3.ReW,error.forecastEI3.ReW)
    t1<-data.frame(forecastE3.ReW,forecastI3.ReW,forecastEI3.ReW)
    n<-which.min(c(mse(forecastE3.ReW,test3),mse(forecastI3.ReW,test3),mse(forecastEI3.ReW,test3)))
    Best.ReW3.error<-t[,n]
    Best.ReW3<-t1[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.ReW3<-colnames(t[n])
    
    t<-data.frame(error.forecastE6.ReW,error.forecastI6.ReW,error.forecastEI6.ReW)
    t1<-data.frame(forecastE6.ReW,forecastI6.ReW,forecastEI6.ReW)
    n<-which.min(c(mse(forecastE6.ReW,test6),mse(forecastI6.ReW,test6),mse(forecastEI6.ReW,test6)))
    Best.ReW6.error<-t[,n]
    Best.ReW6<-t1[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.ReW6<-colnames(t[n])
    
    t<-data.frame(error.forecastE12.ReW,error.forecastI12.ReW,error.forecastEI12.ReW)
    t1<-data.frame(forecastE12.ReW,forecastI12.ReW,forecastEI12.ReW)
    n<-which.min(c(mse(forecastE12.ReW,test12),mse(forecastI12.ReW,test12),mse(forecastEI12.ReW,test12)))
    Best.ReW12.error<-t[,n]
    Best.ReW12<-t1[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.ReW12<-colnames(t[n])
    
    MSE.forecast[j,2]<-mse(Best.RoW1,test1)
    MSE.forecast[j,3]<-mse(Best.ReW1,test1)
    MSE.forecast[j,4]<-mse(random1,test1)
    MSE.forecast[j,5]<-Best.Model.RoW1
    MSE.forecast[j,6]<-Best.Model.ReW1
    MSE.forecast[j,7]<-round(dm.test(Best.RoW1.error, error.random1, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    MSE.forecast[j,8]<-round(dm.test(Best.ReW1.error, error.random1, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    if(mse(Best.RoW1,test1)<mse(Best.ReW1,test1) & MSE.forecast[j,7]<=0.05){
      MSE.forecast[j,9]<-round(dm.test(Best.RoW1.error, Best.ReW1.error, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    }
    if(mse(Best.ReW1,test1)<mse(Best.RoW1,test1) & MSE.forecast[j,8]<=0.05){
      MSE.forecast[j,9]<-round(dm.test(Best.ReW1.error, Best.RoW1.error, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    }
    MSE.forecast[j,10]<-ifelse((mse(Best.ReW1,test1)<mse(Best.RoW1,test1)),"ReW","RoW")
    
    MSE.forecast[j,11]<-mse(Best.RoW3,test3)
    MSE.forecast[j,12]<-mse(Best.ReW3,test3)
    MSE.forecast[j,13]<-mse(random3,test3)
    MSE.forecast[j,14]<-Best.Model.RoW3
    MSE.forecast[j,15]<-Best.Model.ReW3
    MSE.forecast[j,16]<-round(dm.test(Best.RoW3.error, error.random3, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    MSE.forecast[j,17]<-round(dm.test(Best.ReW3.error, error.random3, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    
    if(mse(Best.RoW3,test3)<mse(Best.ReW3,test3) & MSE.forecast[j,16]<=0.05){
      MSE.forecast[j,18]<-round(dm.test(Best.RoW3.error, Best.ReW3.error, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    }
    if(mse(Best.ReW3,test3)<mse(Best.RoW3,test3) & MSE.forecast[j,17]<=0.05){
      MSE.forecast[j,18]<-round(dm.test(Best.ReW3.error, Best.RoW3.error, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    }
    MSE.forecast[j,19]<-ifelse((mse(Best.ReW3,test3)<mse(Best.RoW3,test3)),"ReW","RoW")
    
    MSE.forecast[j,20]<-mse(Best.RoW6,test6)
    MSE.forecast[j,21]<-mse(Best.ReW6,test6)
    MSE.forecast[j,22]<-mse(random6,test6)
    MSE.forecast[j,23]<-Best.Model.RoW6
    MSE.forecast[j,24]<-Best.Model.ReW6
    MSE.forecast[j,25]<-round(dm.test(Best.RoW6.error, error.random6, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    MSE.forecast[j,26]<-round(dm.test(Best.ReW6.error, error.random6, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    if(mse(Best.RoW6,test6)<mse(Best.ReW6,test6) & MSE.forecast[j,25]<=0.05){
      MSE.forecast[j,27]<-round(dm.test(Best.RoW6.error, Best.ReW6.error, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    }
    if(mse(Best.ReW6,test6)<mse(Best.RoW6,test6) & MSE.forecast[j,26]<=0.05){
      MSE.forecast[j,27]<-round(dm.test(Best.ReW6.error, Best.RoW6.error, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    }
    MSE.forecast[j,28]<-ifelse((mse(Best.ReW6,test6)<mse(Best.RoW6,test6)),"ReW","RoW")

    if(length(Best.RoW12)!=0){
    MSE.forecast[j,29]<-mse(Best.RoW12,test12)
    MSE.forecast[j,30]<-mse(Best.ReW12,test12)
    MSE.forecast[j,31]<-mse(random12,test12)
    MSE.forecast[j,32]<-Best.Model.RoW12
    MSE.forecast[j,33]<-Best.Model.ReW12
    MSE.forecast[j,34]<-round(dm.test(Best.RoW12.error, error.random12, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    MSE.forecast[j,35]<-round(dm.test(Best.ReW12.error, error.random12, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    
    if(mse(Best.RoW12,test12)<mse(Best.ReW12,test12)& MSE.forecast[j,34]<=0.05){
      MSE.forecast[j,36]<-round(dm.test(Best.RoW12.error, Best.ReW12.error, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    }
    if(mse(Best.ReW12,test12)<mse(Best.RoW12,test12)& MSE.forecast[j,35]<=0.05){
      MSE.forecast[j,36]<-round(dm.test(Best.ReW12.error, Best.RoW12.error, alternative = c("less"), h = 1,power = 2)$p.value,digits = 4)
    }
    MSE.forecast[j,37]<-ifelse((mse(Best.ReW12,test12)<mse(Best.RoW1,test1)),"ReW","RoW")
    }
    }
  }
MSE.forecast<-data.frame(MSE.forecast)
MSE.forecast$Rep<-as.character(LaenderNamen)
MSE.forecast$number<-1:126
write_excel_csv(MSE.forecast,"MSE.forecast.csv")

f1<-MSE.forecast[,c(1:4)]
f1$f<-1
f3<-MSE.forecast[,c(1,11:13)]
f3$f<-3
f6<-MSE.forecast[,c(1,20:23)]
f6$f<-6
f12<-MSE.forecast[,c(1,29:32)]
f12$f<-12

full<-rbind(melt(f1,id.vars=list("Rep","f")),melt(f3,id.vars=list("Rep","f")),melt(f6,id.vars=list("Rep","f")),melt(f12,id.vars=list("Rep","f")))
full$value<-as.numeric(full$value)

t<-full[full$value==aggregate(full$value[full$f==1],by=list(full$Rep[full$f==1]),min)$x,]
full$imp<-NA
for(i in c(1,3,6,12)){print(i)
  for(j in 1:126){
    country<-LaenderNamen[j]
    d<-subset(full, full$Rep==country &full$f==i)
    full$imp[full$Rep==country&full$f==i]<-100-d$value/d$value[d$variable=="Random1"]*100
  }
}


full<-full[order(full$value),]
full$f<-factor(full$f,levels=unique(full$f))
prog1<-subset(full,full$f==1&full$variable!="Random1")
prog1<-prog1[order(prog1$imp),]
prog1$Rep<-factor(prog1$Rep,levels=unique(prog1$Rep))


a<-ggplot(prog1,aes(x=prog1$imp,y=prog1$Rep))+   #Einige L?nder sind raus, da dort verschlechterung ->Macht Grafik un?bersichtlich
  geom_point(mapping=aes(colour=prog1$variable))+
  labs(x="Verbesserung",y="Land",title="Prognosevergleich", 
       subtitle="Prozentuale Verbesserung gegenüber dem Random Walk (k=1") +
  scale_shape_identity()+
  theme(axis.text.y = element_text(colour="grey20",size=5,angle=0,face="plain"),
        axis.text.x = element_text(colour="grey20",size=5,angle=90,face="plain"))+ 
  geom_vline(xintercept = 0,colour="red")+
  scale_color_manual(name="Modell",labels = c("RoW","ReW"),values = c("blue","#FF9999"))

ggsave("Grundmodell.png",a, width=6, height=9)

d<-MSE.forecast$Rep[MSE.forecast$DM.ReW1>=0.05&MSE.forecast$DM.RoW1>=0.05] #RW gewinnt

data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW1>=0.05&MSE.forecast$DM.RoW1<=0.05]) #RoW schlägt ReW nicht
data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW1<=0.05&MSE.forecast$DM.RoW1>=0.05]) #ReW schlägt RoW nicht
data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW1<=0.05&MSE.forecast$DM.RoW1<=0.05]) #beide schlagen

d1<-prog1
d1$Rep<-as.character(d1$Rep)
prog2<-subset(prog1,prog1$Rep %in% d)

ggplot(prog2,aes(x=prog2$imp,y=prog2$Rep))+   #Einige L?nder sind raus, da dort verschlechterung ->Macht Grafik un?bersichtlich
  geom_point(mapping=aes(colour=prog2$variable))+
  labs(x="Verbesserung",y="Land",title="Prognosevergleich", 
       subtitle="Prozentuale Verbesserung gegenüber dem Random Walk (k=1") +
  scale_shape_identity()+
  theme(axis.text.y = element_text(colour="grey20",size=5,angle=0,face="plain"),
        axis.text.x = element_text(colour="grey20",size=5,angle=90,face="plain"))+ 
  geom_vline(xintercept = 0,colour="red")+
  scale_color_manual(name="Modell",labels = c("RoW","ReW"),values = c("blue","#FF9999"))


MSE.forecastvergleich<-data.frame(matrix(NA,ncol=3,nrow=LaenderNum))
colnames(MSE.forecastvergleich)<-c("Rep",
                          "RoW1","ReW1")
for (j in 1:LaenderNum){print(j)  #rolling window
  RepNum<-LaenderRo[j] 
  Namen<-LaenderNamen[j]
  Data.Land<-data[data$ID==RepNum,]
  if(length(Data.Land$Date)>36){
    forecastEI1.RoW<-NA
    forecastEI1.ReW<-NA
    test1<-NA
    scomb<-0
    ecomb<-0
    for (i in 1:(length(Data.Land$ID)-Start.n)){
      trainRoW<-Data.Land[i:(Start.n-1+i),]  
      trainReW<-Data.Land[1:(Start.n-1+i),]  
      test<-Data.Land[(Start.n+i):length(Data.Land$ID),]
      regressionEIRoW<-lm(dWK.average.log~dValueI.log+dValueE.log,data=trainRoW)
      regressionEIReW<-lm(dWK.average.log~dValueI.log+dValueE.log,data=trainReW)
      
      predEIRoW<-predict(regressionEIRoW,newdata=test)
      predEIReW<-predict(regressionEIReW,newdata=test)
      

      forecastEI1.RoW[i]<-predEIRoW[1]
      forecastEI1.ReW[i]<-predEIReW[1]
      
      test1[i]<-test$dWK.average.log[1]
    }
    
    MSE.forecastvergleich[j,1]<-Namen
   
    MSE.forecastvergleich[j,2]<-mse(forecastEI1.RoW,test1)
    MSE.forecastvergleich[j,3]<-mse(forecastEI1.ReW,test1)
    }
  }

MSE.forecastvergleich<-data.frame(MSE.forecastvergleich)
MSE.forecastvergleich$Rep<-LaenderNamen
MSE.forecastvergleich$number<-1:126

####Grundmodellpaper####
Start.n<-36 #Perioden f?r das erste Modell hier 36 Monate
LaenderRo<-unique(LaenderID$ID)
LaenderNamen<-unique(LaenderID$Rep)
LaenderNum<-length(LaenderRo)

MSE.forecast_USD<-model(data_USD)
MSE.forecast.9303_USD<-model(data9303_USD)
MSE.forecast.0416_USD<-model(data0416_USD)
MSE.forecast.9303_USD<-subset(MSE.forecast.9303_USD,!is.na(MSE.forecast.9303_USD$Random1))
MSE.forecast.0416_USD<-subset(MSE.forecast.0416_USD,!is.na(MSE.forecast.0416_USD$Random1))

prog_USD<-aufbereiten(MSE.forecast_USD)
prog9303_USD<-aufbereiten(MSE.forecast.9303_USD)
prog0416_USD<-aufbereiten(MSE.forecast.0416_USD)

prog9303row_USD<-merge(subset(prog9303_USD,prog9303_USD$variable=="Row1"),MSE.forecast.9303_USD[,c("Rep","DM.RoW1")],by="Rep")
prog9303rew_USD<-merge(subset(prog9303_USD,prog9303_USD$variable=="Rew1"),MSE.forecast.9303_USD[,c("Rep","DM.ReW1")],by="Rep")
prog0416row_USD<-merge(subset(prog0416_USD,prog0416_USD$variable=="Row1"),MSE.forecast.0416_USD[,c("Rep","DM.RoW1")],by="Rep")
prog0416rew_USD<-merge(subset(prog0416_USD,prog0416_USD$variable=="Rew1"),MSE.forecast.0416_USD[,c("Rep","DM.ReW1")],by="Rep")
colnames(prog9303row_USD)<-c(colnames(prog9303row_USD)[-6],"DM")
colnames(prog9303rew_USD)<-c(colnames(prog9303row_USD)[-6],"DM")
colnames(prog0416row_USD)<-c(colnames(prog9303row_USD)[-6],"DM")
colnames(prog0416rew_USD)<-c(colnames(prog9303row_USD)[-6],"DM")

improv_plot(prog9303row_USD,"1993-2003")
improv_plot(prog9303rew_USD,"1993-2003")
improv_plot(prog0416row_USD,"2004-2016")
improv_plot(prog0416rew_USD,"2004-2016")

a<-data.table(paste(MSE.forecast_USD$Rep[MSE.forecast_USD$SupModell=="Random"], collapse = ", "),
           paste(MSE.forecast_USD$Rep[MSE.forecast_USD$SupModell=="RoW"], collapse = ", "),
           paste(MSE.forecast_USD$Rep[MSE.forecast_USD$SupModell=="ReW"], collapse = ", "),
           paste(MSE.forecast_USD$Rep[MSE.forecast_USD$SupModell=="RoW/ReW"], collapse = ", "))

glossaryprint<-xtable(a,label="tab:codebook", caption = 'My Title',include.rownames=F )
colnames(glossaryprint)<-c("Random","RoW","ReW","RoW/ReW")
align(glossaryprint)<-"p{1}p{3,5cm}p{3,5cm}p{3,5cm}p{4cm}" #here is the change
print(glossaryprint,tabular.environment="tabular",floating=FALSE, file = "SupModel9316USD.tex",include.rownames=F )

a<-data.table(paste(MSE.forecast.9303_USD$Rep[MSE.forecast.9303_USD$SupModell=="Random"], collapse = ", "),
              paste(MSE.forecast.9303_USD$Rep[MSE.forecast.9303_USD$SupModell=="RoW"], collapse = ", "),
              paste(MSE.forecast.9303_USD$Rep[MSE.forecast.9303_USD$SupModell=="ReW"], collapse = ", "),
              paste(MSE.forecast.9303_USD$Rep[MSE.forecast.9303_USD$SupModell=="RoW/ReW"], collapse = ", "))

glossaryprint<-xtable(a,label="tab:codebook", caption = 'My Title',include.rownames=F )
colnames(glossaryprint)<-c("Random","RoW","ReW","RoW/ReW")
align(glossaryprint)<-"p{1}p{3,5cm}p{3,5cm}p{3,5cm}p{4cm}" #here is the change
print(glossaryprint,tabular.environment="tabular",floating=FALSE, file = "SupModel9303USD.tex",include.rownames=F )

a<-data.table(paste(MSE.forecast.0416_USD$Rep[MSE.forecast.0416_USD$SupModell=="Random"], collapse = ", "),
              paste(MSE.forecast.0416_USD$Rep[MSE.forecast.0416_USD$SupModell=="RoW"], collapse = ", "),
              paste(MSE.forecast.0416_USD$Rep[MSE.forecast.0416_USD$SupModell=="ReW"], collapse = ", "),
              paste(MSE.forecast.0416_USD$Rep[MSE.forecast.0416_USD$SupModell=="RoW/ReW"], collapse = ", "))
glossaryprint<-xtable(a,label="tab:codebook", caption = 'My Title',include.rownames=F )
colnames(glossaryprint)<-c("Random","RoW","ReW","RoW/ReW")
align(glossaryprint)<-"p{1}p{3,5cm}p{3,5cm}p{3,5cm}p{4cm}" #here is the change
print(glossaryprint,tabular.environment="tabular",floating=FALSE, file = "SupModel0416USD.tex",include.rownames=F )

####1.include p-Values####
prog_USD<-aufbereiten(MSE.forecast_USD)
prog9303_USD<-aufbereiten(MSE.forecast.9303_USD)
prog0416_USD<-aufbereiten(MSE.forecast.0416_USD)

prog9303row_USD<-merge(subset(prog9303_USD,prog9303_USD$variable=="Row1"),MSE.forecast.9303_USD[,c("Rep","DM.RoW1")],by="Rep")
prog9303rew_USD<-merge(subset(prog9303_USD,prog9303_USD$variable=="Rew1"),MSE.forecast.9303_USD[,c("Rep","DM.ReW1")],by="Rep")
prog0416row_USD<-merge(subset(prog0416_USD,prog0416_USD$variable=="Row1"),MSE.forecast.0416_USD[,c("Rep","DM.RoW1")],by="Rep")
prog0416rew_USD<-merge(subset(prog0416_USD,prog0416_USD$variable=="Rew1"),MSE.forecast.0416_USD[,c("Rep","DM.ReW1")],by="Rep")
colnames(prog9303row_USD)<-c(colnames(prog9303row_USD)[-6],"DM")
colnames(prog9303rew_USD)<-c(colnames(prog9303row_USD)[-6],"DM")
colnames(prog0416row_USD)<-c(colnames(prog9303row_USD)[-6],"DM")
colnames(prog0416rew_USD)<-c(colnames(prog9303row_USD)[-6],"DM")

USD1993_2003RoW<-improv_plot(prog9303row_USD,"USD 1993-2003")
USD2004_2016RoW<-improv_plot(prog0416row_USD,"USD 2004-2016")
ggsave("USD_Basic_RoW.png", grid.arrange(USD1993_2003RoW,USD2004_2016RoW, ncol=2), width=12, height=18)

USD1993_2003ReW<-improv_plot(prog9303rew_USD,"USD 1993-2003")
USD2004_2016ReW<-improv_plot(prog0416rew_USD,"USD 2004-2016")
ggsave("USD_Basic_ReW.png", grid.arrange(USD1993_2003ReW,USD2004_2016ReW, ncol=2), width=12, height=11)



####2a.robustness Japan####
#Japan
MSE.forecast_JPY<-model(data_JPY);
MSE.forecast.9303_JPY<-model(data9303_JPY)
MSE.forecast.0416_JPY<-model(data0416_JPY)
MSE.forecast_JPY<-subset(MSE.forecast_JPY,MSE.forecast_JPY$Rep!="Japan")
MSE.forecast.9303_JPY<-subset(MSE.forecast.9303_JPY,MSE.forecast.9303_JPY$Rep!="Japan")
MSE.forecast.0416_JPY<-subset(MSE.forecast.0416_JPY,MSE.forecast.0416_JPY$Rep!="Japan")
MSE.forecast.9303_JPY<-subset(MSE.forecast.9303_JPY,!is.na(MSE.forecast.9303_JPY$Random1))
MSE.forecast.0416_JPY<-subset(MSE.forecast.0416_JPY,!is.na(MSE.forecast.0416_JPY$Random1))

prog_JPY<-aufbereiten(MSE.forecast_JPY)
prog9303_JPY<-aufbereiten(MSE.forecast.9303_JPY)
prog0416_JPY<-aufbereiten(MSE.forecast.0416_JPY)

prog9303row_JPY<-merge(subset(prog9303_JPY,prog9303_JPY$variable=="Row1"),MSE.forecast.9303_JPY[,c("Rep","DM.RoW1")],by="Rep")
prog9303rew_JPY<-merge(subset(prog9303_JPY,prog9303_JPY$variable=="Rew1"),MSE.forecast.9303_JPY[,c("Rep","DM.ReW1")],by="Rep")
prog0416row_JPY<-merge(subset(prog0416_JPY,prog0416_JPY$variable=="Row1"),MSE.forecast.0416_JPY[,c("Rep","DM.RoW1")],by="Rep")
prog0416rew_JPY<-merge(subset(prog0416_JPY,prog0416_JPY$variable=="Rew1"),MSE.forecast.0416_JPY[,c("Rep","DM.ReW1")],by="Rep")
colnames(prog9303row_JPY)<-c(colnames(prog9303row_JPY)[-6],"DM")
colnames(prog9303rew_JPY)<-c(colnames(prog9303row_JPY)[-6],"DM")
colnames(prog0416row_JPY)<-c(colnames(prog9303row_JPY)[-6],"DM")
colnames(prog0416rew_JPY)<-c(colnames(prog9303row_JPY)[-6],"DM")

JPY1993_2003RoW<-improv_plot(prog9303row_JPY,"JPY 1993-2003")
JPY2004_2016RoW<-improv_plot(prog0416row_JPY,"JPY 2004-2016")
ggsave("JPY_Basic_RoW.png", grid.arrange(JPY1993_2003RoW,JPY2004_2016RoW, ncol=2), width=12, height=11)

improv_plot(prog9303rew_JPY,"JPY 1993-2003")
improv_plot(prog0416rew_JPY,"JPY 2004-2016")

a<-data.table(paste(MSE.forecast_JPY$Rep[MSE.forecast_JPY$SupModell=="Random"], collapse = ", "),
              paste(MSE.forecast_JPY$Rep[MSE.forecast_JPY$SupModell=="RoW"], collapse = ", "),
              paste(MSE.forecast_JPY$Rep[MSE.forecast_JPY$SupModell=="ReW"], collapse = ", "),
              paste(MSE.forecast_JPY$Rep[MSE.forecast_JPY$SupModell=="RoW/ReW"], collapse = ", "))
glossaryprint<-xtable(a,label="tab:codebook", caption = 'My Title',include.rownames=F )
colnames(glossaryprint)<-c("Random","RoW","ReW","RoW/ReW")
align(glossaryprint)<-"p{1}p{3,5cm}p{3,5cm}p{3,5cm}p{4cm}" #here is the change
print(glossaryprint,tabular.environment="tabular",floating=FALSE, file = "SupModel9316_JPY.tex",include.rownames=F )

a<-data.table(paste(MSE.forecast.9303_JPY$Rep[MSE.forecast.9303_JPY$SupModell=="Random"], collapse = ", "),
              paste(MSE.forecast.9303_JPY$Rep[MSE.forecast.9303_JPY$SupModell=="RoW"], collapse = ", "),
              paste(MSE.forecast.9303_JPY$Rep[MSE.forecast.9303_JPY$SupModell=="ReW"], collapse = ", "),
              paste(MSE.forecast.9303_JPY$Rep[MSE.forecast.9303_JPY$SupModell=="RoW/ReW"], collapse = ", "))
glossaryprint<-xtable(a,label="tab:codebook", caption = 'My Title',include.rownames=F )
colnames(glossaryprint)<-c("Random","RoW","ReW","RoW/ReW")
align(glossaryprint)<-"p{1}p{3,5cm}p{3,5cm}p{3,5cm}p{4cm}" #here is the change
print(glossaryprint,tabular.environment="tabular",floating=FALSE, file = "SupModel9303_JPY.tex",include.rownames=F )

a<-data.table(paste(MSE.forecast.0416_JPY$Rep[MSE.forecast.0416_JPY$SupModell=="Random"], collapse = ", "),
              paste(MSE.forecast.0416_JPY$Rep[MSE.forecast.0416_JPY$SupModell=="RoW"], collapse = ", "),
              paste(MSE.forecast.0416_JPY$Rep[MSE.forecast.0416_JPY$SupModell=="ReW"], collapse = ", "),
              paste(MSE.forecast.0416_JPY$Rep[MSE.forecast.0416_JPY$SupModell=="RoW/ReW"], collapse = ", "))
glossaryprint<-xtable(a,label="tab:codebook", caption = 'My Title',include.rownames=F )
colnames(glossaryprint)<-c("Random","RoW","ReW","RoW/ReW")
align(glossaryprint)<-"p{1}p{3,5cm}p{3,5cm}p{3,5cm}p{4cm}" #here is the change
print(glossaryprint,tabular.environment="tabular",floating=FALSE, file = "SupModel0416_JPY.tex",include.rownames=F )

####2b.robustness Pound####
MSE.forecast_GBP<-model(data_GBP);
MSE.forecast.9303_GBP<-model(data9303_GBP)
MSE.forecast.0416_GBP<-model(data0416_GBP)
MSE.forecast_GBP<-subset(MSE.forecast_GBP,MSE.forecast_GBP$Rep!="United Kingdom")
MSE.forecast.9303_GBP<-subset(MSE.forecast.9303_GBP,MSE.forecast.9303_GBP$Rep!="United Kingdom")
MSE.forecast.0416_GBP<-subset(MSE.forecast.0416_GBP,MSE.forecast.0416_GBP$Rep!="United Kingdom")
MSE.forecast.9303_GBP<-subset(MSE.forecast.9303_GBP,!is.na(MSE.forecast.9303_GBP$Random1))
MSE.forecast.0416_GBP<-subset(MSE.forecast.0416_GBP,!is.na(MSE.forecast.0416_GBP$Random1))

prog_GBP<-aufbereiten(MSE.forecast_GBP)
prog9303_GBP<-aufbereiten(MSE.forecast.9303_GBP)
prog0416_GBP<-aufbereiten(MSE.forecast.0416_GBP)

prog9303row_GBP<-merge(subset(prog9303_GBP,prog9303_GBP$variable=="Row1"),MSE.forecast.9303_GBP[,c("Rep","DM.RoW1")],by="Rep")
prog9303rew_GBP<-merge(subset(prog9303_GBP,prog9303_GBP$variable=="Rew1"),MSE.forecast.9303_GBP[,c("Rep","DM.ReW1")],by="Rep")
prog0416row_GBP<-merge(subset(prog0416_GBP,prog0416_GBP$variable=="Row1"),MSE.forecast.0416_GBP[,c("Rep","DM.RoW1")],by="Rep")
prog0416rew_GBP<-merge(subset(prog0416_GBP,prog0416_GBP$variable=="Rew1"),MSE.forecast.0416_GBP[,c("Rep","DM.ReW1")],by="Rep")
colnames(prog9303row_GBP)<-c(colnames(prog9303row_GBP)[-6],"DM")
colnames(prog9303rew_GBP)<-c(colnames(prog9303row_GBP)[-6],"DM")
colnames(prog0416row_GBP)<-c(colnames(prog9303row_GBP)[-6],"DM")
colnames(prog0416rew_GBP)<-c(colnames(prog9303row_GBP)[-6],"DM")


improv_plot(prog9303rew_GBP,"GBP 1993-2003")
improv_plot(prog0416rew_GBP,"GBP 2004-2016")


GBP1993_2003RoW<-improv_plot(prog9303row_GBP,"GBP 1993-2003")
GBP2004_2016RoW<-improv_plot(prog0416row_GBP,"GBP 2004-2016")
ggsave("GBP_Basic_RoW.png", grid.arrange(GBP1993_2003RoW,GBP2004_2016RoW, ncol=2), width=12, height=11)


a<-data.table(paste(MSE.forecast_GBP$Rep[MSE.forecast_GBP$SupModell=="Random"], collapse = ", "),
              paste(MSE.forecast_GBP$Rep[MSE.forecast_GBP$SupModell=="RoW"], collapse = ", "),
              paste(MSE.forecast_GBP$Rep[MSE.forecast_GBP$SupModell=="ReW"], collapse = ", "),
              paste(MSE.forecast_GBP$Rep[MSE.forecast_GBP$SupModell=="RoW/ReW"], collapse = ", "))
glossaryprint<-xtable(a,label="tab:codebook", caption = 'My Title',include.rownames=F )
colnames(glossaryprint)<-c("Random","RoW","ReW","RoW/ReW")
align(glossaryprint)<-"p{1}p{3,5cm}p{3,5cm}p{3,5cm}p{4cm}" #here is the change
print(glossaryprint,tabular.environment="tabular",floating=FALSE, file = "SupModel9316_GBP.tex",include.rownames=F )

a<-data.table(paste(MSE.forecast.9303_GBP$Rep[MSE.forecast.9303_GBP$SupModell=="Random"], collapse = ", "),
              paste(MSE.forecast.9303_GBP$Rep[MSE.forecast.9303_GBP$SupModell=="RoW"], collapse = ", "),
              paste(MSE.forecast.9303_GBP$Rep[MSE.forecast.9303_GBP$SupModell=="ReW"], collapse = ", "),
              paste(MSE.forecast.9303_GBP$Rep[MSE.forecast.9303_GBP$SupModell=="RoW/ReW"], collapse = ", "))
glossaryprint<-xtable(a,label="tab:codebook", caption = 'My Title',include.rownames=F )
colnames(glossaryprint)<-c("Random","RoW","ReW","RoW/ReW")
align(glossaryprint)<-"p{1}p{3,5cm}p{3,5cm}p{3,5cm}p{4cm}" #here is the change
print(glossaryprint,tabular.environment="tabular",floating=FALSE, file = "SupModel9303_GBP.tex",include.rownames=F )

a<-data.table(paste(MSE.forecast.0416_GBP$Rep[MSE.forecast.0416_GBP$SupModell=="Random"], collapse = ", "),
              paste(MSE.forecast.0416_GBP$Rep[MSE.forecast.0416_GBP$SupModell=="RoW"], collapse = ", "),
              paste(MSE.forecast.0416_GBP$Rep[MSE.forecast.0416_GBP$SupModell=="ReW"], collapse = ", "),
              paste(MSE.forecast.0416_GBP$Rep[MSE.forecast.0416_GBP$SupModell=="RoW/ReW"], collapse = ", "))
glossaryprint<-xtable(a,label="tab:codebook", caption = 'My Title',include.rownames=F )
colnames(glossaryprint)<-c("Random","RoW","ReW","RoW/ReW")
align(glossaryprint)<-"p{1}p{3,5cm}p{3,5cm}p{3,5cm}p{4cm}" #here is the change
print(glossaryprint,tabular.environment="tabular",floating=FALSE, file = "SupModel0416_GBP.tex",include.rownames=F )


t<-merge(MSE.forecast.9303_JPY[,c("Rep","DM.RoW1")],MSE.forecast.9303_GBP[,c("Rep","DM.RoW1")],by="Rep")
colnames(t)<-c("Rep","JPY","GBP")
MSE_vergleich9303<-melt(t)
MSE_vergleich9303<-merge(MSE_vergleich9303,MSE.forecast.9303_USD[,c("Rep","DM.RoW1")],by="Rep",all.x=TRUE)
colnames(MSE_vergleich9303)<-c("Rep","Currency","DM.Currency","DM.USD")

t<-merge(MSE.forecast.0416_JPY[,c("Rep","DM.RoW1")],MSE.forecast.0416_GBP[,c("Rep","DM.RoW1")],by="Rep")
colnames(t)<-c("Rep","JPY","GBP")
MSE_vergleich0416<-melt(t)
MSE_vergleich0416<-merge(MSE_vergleich0416,MSE.forecast.0416_USD[,c("Rep","DM.RoW1")],by="Rep",all.x=TRUE)
colnames(MSE_vergleich0416)<-c("Rep","Currency","DM.Currency","DM.USD")

Vergleich93<-ggplot(MSE_vergleich9303,aes(y=DM.USD, x=DM.Currency, color=Currency))+
  labs(y="DM.USD",x="DM.JPY/DM.GBP",subtitle="1993-2003")+
  geom_point(aes(alpha=0.9))+
  geom_abline(slope=1, intercept=0,linetype=2)+
  guides(alpha=FALSE)+
  coord_cartesian(xlim=c(0,0.6),ylim=c(0, 1))+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(size=.1, color="grey" ),
        panel.grid.major.x = element_line(size=.1, color="grey" ),
        legend.justification=c(1,0), legend.position=c(0.5,0.7),
        axis.text.x = element_text(colour="grey20",face="plain"),
        axis.title.x=element_blank())

Vergleich04<-ggplot(MSE_vergleich0416,aes(y=DM.USD, x=DM.Currency, color=Currency))+
  labs(y="DM.USD",x="DM.JPY/DM.GBP",subtitle="2004-2016")+
  geom_point(aes(alpha=0.9))+
  geom_abline(slope=1, intercept=0,linetype=2)+
  guides(alpha=FALSE)+
  coord_cartesian(xlim=c(0,0.6),ylim=c(0, 1))+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(size=.1, color="grey" ),
        panel.grid.major.x = element_line(size=.1, color="grey" ),
        legend.position="none",
        axis.text.x = element_text(colour="grey20",face="plain"))
ggsave("Currency_Vergleich.png", grid.arrange(Vergleich93,Vergleich04))

####3.drift####
dm.test
MSE.forecast.9316.drift<-model_drift(data_USD)
MSE.forecast.9303.drift<-model_drift(data9303_USD)
MSE.forecast.0416.drift<-model_drift(data0416_USD)

MSE.forecast.9316.drift<-MSE.forecast.9316.drift[complete.cases(MSE.forecast.9316.drift),]
MSE.forecast.9303.drift<-MSE.forecast.9303.drift[complete.cases(MSE.forecast.9303.drift),]
MSE.forecast.0416.drift<-MSE.forecast.0416.drift[complete.cases(MSE.forecast.0416.drift),]
pt(-3.74,100)

glossaryprint<-xtable(MSE.forecast.9303.drift[,-c(2,3,4,5)],label="tab:codebook", caption = 'My Title',include.rownames=F )
colnames(glossaryprint)<-c("Country","DM.statistic RW","DM.p.value RW","DM.statistic RW+Drift","DM.p.value RW+Drift")
print(glossaryprint,tabular.environment="tabular",floating=FALSE, file = "Drift9303.tex",include.rownames=F )

glossaryprint<-xtable(MSE.forecast.0416.drift[,-c(2,3,4,5)],label="tab:codebook", caption = 'My Title',include.rownames=F )
colnames(glossaryprint)<-c("Country","DM.statistic RW","DM.p.value RW","DM.statistic RW+Drift","DM.p.value RW+Drift")
print(glossaryprint,tabular.environment="tabular",floating=FALSE, file = "Drift0416.tex",include.rownames=F )

prog9303row_USD_Drift<-merge(subset(prog9303_USD,prog9303_USD$variable=="Row1"),MSE.forecast.9303_USD[,c("Rep","DM.RoW.Drift")],by="Rep")
prog9303rew_USD_Drift<-merge(subset(prog9303_USD,prog9303_USD$variable=="Rew1"),MSE.forecast.9303_USD[,c("Rep","DM.ReW.Drift")],by="Rep")
prog0416row_USD_Drift<-merge(subset(prog0416_USD,prog0416_USD$variable=="Row1"),MSE.forecast.0416_USD[,c("Rep","DM.RoW.Drift")],by="Rep")
prog0416rew_USD_Drift<-merge(subset(prog0416_USD,prog0416_USD$variable=="Rew1"),MSE.forecast.0416_USD[,c("Rep","DM.ReW.Drift")],by="Rep")
colnames(prog9303row_USD_Drift)<-c(colnames(prog9303row_USD)[-6],"DM")
colnames(prog9303rew_USD_Drift)<-c(colnames(prog9303row_USD)[-6],"DM")
colnames(prog0416row_USD_Drift)<-c(colnames(prog9303row_USD)[-6],"DM")
colnames(prog0416rew_USD_Drift)<-c(colnames(prog9303row_USD)[-6],"DM")

improv_plot(prog9303row_USD_Drift,"USD 1993-2003")
ggsave("USD 1993-2003 RoW Drift.pdf")
improv_plot(prog9303rew_USD_Drift,"USD 1993-2003")
ggsave("USD 1993-2003 ReW Drift.pdf")
improv_plot(prog0416row_USD_Drift,"USD 2004-2016")
ggsave("USD 2004-2016 RoW Drift.pdf")
improv_plot(prog0416rew_USD_Drift,"USD 2004-2016")
ggsave("USD 2004-2016 ReW Drift.pdf")

####4.Clark and West####
test11<-model_clarkandwest(data_USD)
test12<-model_clarkandwest(data0416_USD)
test13<-model_clarkandwest(data9303_USD)

test12<-test12[complete.cases(test12),]
test13<-test13[complete.cases(test13),]

mean(test11$CW_RoW)
mean(test11$DM.RoW1)
median(test11$CW_RoW)
median(test11$DM.RoW1)

median(test12$CW_RoW)
median(test12$DM.RoW1)

median(test13$CW_RoW)
median(test13$DM.RoW1)

test11$Rep<-LaenderNamen
test12$Rep<-LaenderNamen

plot(test12$CW_RoW,test12$DM.RoW1)
###5.Sign forecast####
data_USD_sign<-data_USD

model_sign<-function(Daten){forecast<-data.frame(matrix(NA,ncol=6,nrow=LaenderNum))
colnames(forecast)<-c("Rep","Row1","Rew1","RoW1.model","ReW1.model","Random")
Daten$dWK.average.log[Daten$dWK.average.log>0&!is.na(Daten$dWK.average.log)]<-1#rise
Daten$dWK.average.log[Daten$dWK.average.log<0&!is.na(Daten$dWK.average.log)]<-0#fall
Daten$dWK.average.log.lag1[Daten$dWK.average.log.lag1>0&!is.na(Daten$dWK.average.log.lag1)]<-1#rise
Daten$dWK.average.log.lag1[Daten$dWK.average.log.lag1<0&!is.na(Daten$dWK.average.log.lag1)]<-0#fall
for (j in 1:LaenderNum){print(j)  #j loop through all countries
  RepNum<-LaenderRo[j] # Number of Specific country (from Comstat)
  Namen<-LaenderNamen[j] #Name of Country j
  Data.Land<-Daten[Daten$ID==RepNum,] #Extract country data
  if(length(Data.Land$Date)>36){ #just forecast if there is enough data (>36 data points)
    forecastE1.RoW<-NA #create basic variables
    forecastE1.ReW<-NA
    forecastI1.RoW<-NA
    forecastI1.ReW<-NA
    forecastEI1.RoW<-NA
    forecastEI1.ReW<-NA
    test1<-NA
    forecast.random<-NA
    error.forecast.random<-NA
    
    error.forecastE1.RoW<-NA
    error.forecastE1.ReW<-NA
    error.forecastI1.RoW<-NA
    error.forecastI1.ReW<-NA
    error.forecastEI1.RoW<-NA
    error.forecastEI1.ReW<-NA
   
    for (i in 1:(length(Data.Land$ID)-Start.n)){ #loop through all available periods
      trainRoW<-Data.Land[i:(Start.n-1+i),]  #Training data in Period i (RoW)
      trainReW<-Data.Land[1:(Start.n-1+i),]  #Training data in Period i (ReW)
      test<-Data.Land[(Start.n+i):length(Data.Land$ID),] #Test data in Period i
      
      regressionERoW<-glm(dWK.average.log~dValueE.log,data=trainRoW, family=binomial) #Train different model
      regressionIRoW<-glm(dWK.average.log~dValueI.log,data=trainRoW, family=binomial)
      regressionEIRoW<-glm(dWK.average.log~dValueE.log+dValueI.log,data=trainRoW, family=binomial)
      regressionEReW<-glm(dWK.average.log~dValueE.log,data=trainReW, family=binomial)
      regressionIReW<-glm(dWK.average.log~dValueI.log,data=trainReW, family=binomial)
      regressionEIReW<-glm(dWK.average.log~dValueE.log+dValueI.log,data=trainReW, family=binomial)
       
      predERoW<-predict(regressionERoW,newdata=test, type="response") #Predict next Values 
      predIRoW<-predict(regressionIRoW,newdata=test, type="response")
      predEIRoW<-predict(regressionEIRoW,newdata=test, type="response")
      predEReW<-predict(regressionEReW,newdata=test, type="response")
      predIReW<-predict(regressionIReW,newdata=test, type="response")
      predEIReW<-predict(regressionEIReW,newdata=test, type="response")
      
      forecastE1.RoW[i]<-predERoW[1] #take forecast for t+1 (could be shortend)
      forecastI1.RoW[i]<-predIRoW[1]
      forecastEI1.RoW[i]<-predEIRoW[1]
      forecastE1.ReW[i]<-predEReW[1]
      forecastI1.ReW[i]<-predIReW[1]
      forecastEI1.ReW[i]<-predEIReW[1]
      
      forecast.random[i]<-test$dWK.average.log.lag1[1]
      
      test1[i]<-test$dWK.average.log[1] #actual value
      
    }
    
    forecastE1.RoW[forecastE1.RoW>=0.5]<-1
    forecastE1.RoW[forecastE1.RoW<0.5]<-0
    forecastI1.RoW[forecastI1.RoW>=0.5]<-1
    forecastI1.RoW[forecastI1.RoW<0.5]<-0
    forecastEI1.RoW[forecastEI1.RoW>=0.5]<-1
    forecastEI1.RoW[forecastEI1.RoW<0.5]<-0
    forecastE1.ReW[forecastE1.ReW>=0.5]<-1
    forecastE1.ReW[forecastE1.ReW<0.5]<-0
    forecastI1.ReW[forecastI1.ReW>=0.5]<-1
    forecastI1.ReW[forecastI1.ReW<0.5]<-0
    forecastEI1.ReW[forecastEI1.ReW>=0.5]<-1
    forecastEI1.ReW[forecastEI1.ReW<0.5]<-0
    
    forecast[j,1]<-Namen #not needed?
    
    error.forecastE1.RoW<-1-mean(abs(forecastE1.RoW-test1)) #Prediction errors
    error.forecastI1.RoW<-1-mean(abs(forecastI1.RoW-test1))
    error.forecastEI1.RoW<-1-mean(abs(forecastEI1.RoW-test1))
    error.forecastE1.ReW<-1-mean(abs(forecastE1.ReW-test1))
    error.forecastI1.ReW<-1-mean(abs(forecastI1.ReW-test1))
    error.forecastEI1.ReW<-1-mean(abs(forecastEI1.ReW-test1))
    
    error.forecast.random<-1-mean(abs(forecast.random-test1))
    
    t<-data.frame(error.forecastE1.RoW,error.forecastI1.RoW,error.forecastEI1.RoW)
    n<-which.max(t)
    Best.RoW1.error<-t[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.RoW1<-colnames(t[n])
    
    t<-data.frame(error.forecastE1.ReW,error.forecastI1.ReW,error.forecastEI1.ReW)
    n<-which.max(t)
    Best.ReW1.error<-t[,n]
    colnames(t)<-c("Export","Import","Export und Import")
    Best.Model.ReW1<-colnames(t[n])
    
    forecast[j,2]<-Best.RoW1.error
    forecast[j,3]<-Best.ReW1.error
    forecast[j,4]<-Best.Model.RoW1
    forecast[j,5]<-Best.Model.ReW1
    forecast[j,6]<-error.forecast.random
  }
}
forecast<-data.frame(forecast)
forecast$Rep<-as.character(LaenderNamen)
return(forecast)
}

MSE.forecast_USD_sign<-model_sign(data_USD)
MSE.forecast.9303_USD_sign<-model_sign(data9303_USD)
MSE.forecast.0416_USD_sign<-model_sign(data0416_USD)

for(i in LaenderNamen){
  if(!is.na(mean(data0416_USD$dWK.average.log[data0416_USD$Rep==i]))){
    plot(data0416_USD$dWK.average.log[data0416_USD$Rep==i])
    invisible(readline(prompt="Press [enter] to continue"))
  }
}



MSE.forecast_USD_sign.clean<-MSE.forecast_USD_sign[complete.cases(MSE.forecast_USD_sign),]
length(MSE.forecast_USD_sign.clean[MSE.forecast_USD_sign.clean$Row1>0.5,]$Rep)
length(MSE.forecast_USD_sign.clean[MSE.forecast_USD_sign.clean$Row1<0.5,]$Rep)

mean(MSE.forecast_USD_sign.clean$Row1)
mean(MSE.forecast.9303_USD_sign.clean$Row1)
mean(MSE.forecast.0416_USD_sign.clean$Row1)

median(MSE.forecast_USD_sign.clean$Row1)
median(MSE.forecast.9303_USD_sign.clean$Row1)
median(MSE.forecast.0416_USD_sign.clean$Row1)


MSE.forecast.9303_USD_sign.clean<-MSE.forecast.9303_USD_sign[complete.cases(MSE.forecast.9303_USD_sign),]
length(MSE.forecast.9303_USD_sign.clean[MSE.forecast.9303_USD_sign.clean$Row1>MSE.forecast.9303_USD_sign.clean$Random,]$Rep)
length(MSE.forecast.9303_USD_sign.clean[MSE.forecast.9303_USD_sign.clean$Row1<MSE.forecast.9303_USD_sign.clean$Random,]$Rep)

MSE.forecast.0416_USD_sign.clean<-MSE.forecast.0416_USD_sign[complete.cases(MSE.forecast.0416_USD_sign),]
length(MSE.forecast.0416_USD_sign.clean[MSE.forecast.0416_USD_sign.clean$Row1>MSE.forecast.0416_USD_sign.clean$Random,]$Rep)
length(MSE.forecast.0416_USD_sign.clean[MSE.forecast.0416_USD_sign.clean$Row1<MSE.forecast.0416_USD_sign.clean$Random,]$Rep)

ggplot(MSE.forecast_USD_sign,aes(Random,Row1))+
  geom_point()+
  labs(x="Random Walk forecast",y="RoW forecast")+
  geom_abline(slope=1,intercept=0)+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(size=.1, color="grey" ),
        panel.grid.major.x = element_line(size=.1, color="grey" ))
ggsave("USD 1993-2016 RoW Sign.pdf")

ggplot(MSE.forecast_USD_sign,aes(Random,Rew1))+
  geom_point()+
  labs(x="Random Walk forecast",y="ReW forecast")+
  geom_abline(slope=1,intercept=0)+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(size=.1, color="grey" ),
        panel.grid.major.x = element_line(size=.1, color="grey" ))
ggsave("USD 1993-2016 ReW Sign.pdf")

ggplot(MSE.forecast.9303_USD_sign,aes(Random,Row1))+
  geom_point()+
  labs(x="Random Walk forecast",y="RoW forecast")+
  geom_abline(slope=1,intercept=0)+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(size=.1, color="grey" ),
        panel.grid.major.x = element_line(size=.1, color="grey" ))
ggsave("USD 1993-2016 RoW Sign.pdf")

ggplot(MSE.forecast.0416_USD_sign,aes(Random,Rew1))+
  geom_point()+
  labs(x="Random Walk forecast",y="ReW forecast")+
  geom_abline(slope=1,intercept=0)+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(size=.1, color="grey" ),
        panel.grid.major.x = element_line(size=.1, color="grey" ))
ggsave("USD 1993-2016 ReW Sign.pdf")

plot(MSE.forecast_USD_sign$Random,MSE.forecast_USD_sign$Row1)
lines(seq(0,1,0.1),seq(0,1,0.1))

####6.Interest rate differentials####

Interest_Rates <- read_excel("C:/Users/Baumg?rtner/Desktop/Masterarbeit/Masterarbeit/Daten/Interest Rates/Interest Rates_OECD_modified.xls.xlsx", 
                                               col_types = c("text", "date", "numeric"))

t<-spread(Interest_Rates,key = "Rep", value = "i")
t1<-apply(t[, 2:44], 2, function (col) col - t[,"United States"])
t1<-data.frame(t1)
colnames(t1)<-colnames(t)[-1]
Interest_Rates<-cbind(unique(Interest_Rates$Date),t1)
colnames(Interest_Rates)[1]<-"Date"
Interest_Rates<-melt(Interest_Rates,id=c("Date"))
colnames(Interest_Rates)<-c("Date","Rep","i")
Interest_Rates<-merge(LaenderID,Interest_Rates,by="Rep",all.y  =TRUE)

Daten_interest<-merge(data_USD,Interest_Rates,by=c("Rep","Date","ID"))
Daten_interest$i<-ave(Daten_interest$i,factor(Daten_interest$Rep),FUN=function(x) na.approx(x,maxgap = 6))

Daten_interest$li<-Daten_interest$i #log geht nicht weil negative Werte

Daten_interest$di.log<-ave(Daten_interest$li,factor(Daten_interest$Rep),FUN=function(x) c(NA,diff(x)))

unique(MSE.forecast.interest_USD.clean$Rep)

Daten_interest<-within(Daten_interest, di.log.lag1 <- ave(Daten_interest$di.log,factor(Daten_interest$ID),FUN=function(x) dplyr::lag(x,1)))
Daten_interest<-within(Daten_interest, di.log.lag3 <- ave(Daten_interest$di.log,factor(Daten_interest$ID),FUN=function(x) dplyr::lag(x,3)))
Daten_interest<-within(Daten_interest, di.log.lag6 <- ave(Daten_interest$di.log,factor(Daten_interest$ID),FUN=function(x) dplyr::lag(x,6)))
Daten_interest<-within(Daten_interest, di.log.lag12 <- ave(Daten_interest$di.log,factor(Daten_interest$ID),FUN=function(x) dplyr::lag(x,12)))



MSE.forecast.interest_USD<-model_interest(Daten_interest)

MSE.forecast.interest_USD.clean<-MSE.forecast.interest_USD[complete.cases(MSE.forecast.interest_USD),-2]


glossaryprint<-xtable(MSE.forecast.interest_USD.clean,label="tab:codebook", caption = 'My Title',include.rownames=F )
colnames(glossaryprint)<-c("Country","Commodity Model","Interest DM statistic","Interest p-Value","Interest + Commodity DM statistic","Interest + Commodity p-value")
align(glossaryprint)<-"p{1}p{3,5cm}p{3,5cm}p{3,5cm}p{4cm}" #here is the change
print(glossaryprint,tabular.environment="tabular",floating=FALSE, file = "CarryUSD.tex",include.rownames=F )


#####Grafiken####

#Darstellung CV
x<-as.numeric(rep(1:30,times=6))
t<-as.numeric(rep(1:6,each=30))
rew<-c("train","train","train","train","test",rep("",times=25),
       "train","train","train","train","train","test",rep("",times=24),
       "train","train","train","train","train","train","test",rep("",times=23),
       "train","train","train","train","train","train","train","test",rep("",times=22),
       "train","train","train","train","train","train","train","train","test",rep("",times=21),
       "train","train","train","train","train","train","train","train","train","test",rep("",times=20))

row<-c("train","train","train","train","test",rep("",times=25),
       "","train","train","train","train","test",rep("",times=24),
       "","","train","train","train","train","test",rep("",times=23),
       "","","","train","train","train","train","test",rep("",times=22),
       "","","","","train","train","train","train","test",rep("",times=21),
       "","","","","","train","train","train","train","test",rep("",times=20))

dd<-data.frame(x,t,rew,row)
dd$rew<-factor(dd$rew)
dd$row<-factor(dd$row)

a<-ggplot(dd,aes(dd$x,dd$t,group=dd$t))+
  geom_point(mapping=aes(colour=dd$rew,size=2))+
  scale_y_reverse( lim=c(6,1))+
  labs(x="Observation",y="Periode",title="Recursive Window Modell")+
  scale_color_manual(name="Model",labels = c("NA","forecast periods","training data"),values = c("grey","Red","dodgerblue4"))+
  guides(size= FALSE) 

b<-ggplot(dd,aes(dd$x,dd$t,group=dd$t))+
  geom_point(mapping=aes(colour=dd$row,size=2))+
  scale_y_reverse( lim=c(6,1))+
  labs(x="Observation",y="Periode",title="Rolling Window Modell")+
  scale_color_manual(name="Model",labels = c("NA","forecast periods","training data"),values = c("grey","Red","dodgerblue4"))+
  guides(size= FALSE) 

ggsave("ReWRoWModell.png", grid.arrange(a,b))

##L?nder?bersicht

a<-merge(aggregate(data$Date,by=list(data$Rep),min),aggregate(data$Date,by=list(data$Rep),max),by="Group.1")
b<-merge(a,aggregate(data$Date,by=list(data$Rep),length),by="Group.1")
write.csv2(b,"Zeitrum?bersicht.csv2")

data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW1>=0.05&MSE.forecast$DM.RoW1>=0.05]) #RW gewinnt
data.frame(MSE.forecast$Rep[MSE.forecast$DM.Vergleich<=0.05&MSE.forecast$DM.RoW1<=0.05]) #RoW schlägt ReW nicht
data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW1<=0.05&MSE.forecast$DM.RoW1>=0.05]) #ReW schlägt RoW nicht
data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW1<=0.05&MSE.forecast$DM.RoW1<=0.05]) #beide schlagen

data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW1<=0.05|MSE.forecast$DM.RoW1<=0.05])
data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW1<=0.05&MSE.forecast$DM.RoW1<=0.05])

data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW3>=0.05&MSE.forecast$DM.RoW3>=0.05]) #RW gewinnt
data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW3<=0.05|MSE.forecast$DM.RoW3<=0.05])
data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW3<=0.05&MSE.forecast$DM.RoW3<=0.05])

data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW6>=0.05&MSE.forecast$DM.RoW6>=0.05]) #RW gewinnt
data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW6<=0.05|MSE.forecast$DM.RoW6<=0.05])
data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW6<=0.05&MSE.forecast$DM.RoW6<=0.05])

data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW12>=0.05&MSE.forecast$DM.RoW12>=0.05]) #RW gewinnt
data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW12<=0.05|MSE.forecast$DM.RoW12<=0.05])
data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW12<=0.05&MSE.forecast$DM.RoW12<=0.05])

a<-data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW1>=0.05&MSE.forecast$DM.RoW1>=0.05]) #RW gewinnt
b<-data.frame(MSE.forecast$Rep[MSE.forecast$Row1<=MSE.forecast$Rew1& MSE.forecast$DM.RoW1<=0.05 & MSE.forecast$Vergleich1<=0.05]) #RoW gewinnt
c<-data.frame(MSE.forecast$Rep[MSE.forecast$Row1>=MSE.forecast$Rew1& MSE.forecast$DM.ReW1<=0.05 & MSE.forecast$Vergleich1<=0.05]) #ReW gewinnt

d<-data.frame(MSE.forecast$Rep[MSE.forecast$Row1<=MSE.forecast$Rew1& MSE.forecast$DM.RoW1<=0.05 & MSE.forecast$Vergleich1>=0.05])
e<-data.frame(MSE.forecast$Rep[MSE.forecast$Row1>=MSE.forecast$Rew1& MSE.forecast$DM.ReW1<=0.05 & MSE.forecast$Vergleich1>=0.05])
write_excel_csv(a,"a.csv")
write_excel_csv(b,"b.csv")
write_excel_csv(c,"c.csv")
write_excel_csv(d,"d.csv")
write_excel_csv(e,"e.csv")

a<-data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW3>=0.05&MSE.forecast$DM.RoW3>=0.05]) #RW gewinnt
b<-data.frame(MSE.forecast$Rep[MSE.forecast$Row3<=MSE.forecast$Rew3& MSE.forecast$DM.RoW3<=0.05 & MSE.forecast$Vergleich3<=0.05]) #RoW gewinnt
c<-data.frame(MSE.forecast$Rep[MSE.forecast$Row3>=MSE.forecast$Rew3& MSE.forecast$DM.ReW3<=0.05 & MSE.forecast$Vergleich3<=0.05]) #ReW gewinnt
d<-data.frame(MSE.forecast$Rep[MSE.forecast$Row3<=MSE.forecast$Rew3& MSE.forecast$DM.RoW3<=0.05 & MSE.forecast$Vergleich3>=0.05])
e<-data.frame(MSE.forecast$Rep[MSE.forecast$Row3>=MSE.forecast$Rew3& MSE.forecast$DM.ReW3<=0.05 & MSE.forecast$Vergleich3>=0.05])

a<-data.frame(MSE.forecast$Rep[MSE.forecast$DM.ReW6>=0.05&MSE.forecast$DM.RoW6>=0.05]) #RW gewinnt
b<-data.frame(MSE.forecast$Rep[MSE.forecast$Row6<=MSE.forecast$Rew6& MSE.forecast$DM.RoW6<=0.05 & MSE.forecast$Vergleich6<=0.05]) #RoW gewinnt
c<-data.frame(MSE.forecast$Rep[MSE.forecast$Row6>=MSE.forecast$Rew6& MSE.forecast$DM.ReW6<=0.05 & MSE.forecast$Vergleich6<=0.05]) #ReW gewinnt
d<-data.frame(MSE.forecast$Rep[MSE.forecast$Row6<=MSE.forecast$Rew6& MSE.forecast$DM.RoW6<=0.05 & MSE.forecast$Vergleich6>=0.05])
e<-data.frame(MSE.forecast$Rep[MSE.forecast$Row6>=MSE.forecast$Rew6& MSE.forecast$DM.ReW6<=0.05 & MSE.forecast$Vergleich6>=0.05])


intersect(MSE.forecastE$Rep[MSE.forecastE$DM1.ReW>=0.05&MSE.forecastE$DM1.RoW>=0.05],MSE.forecastI$Rep[MSE.forecastI$DM1.ReW>=0.05&MSE.forecastI$DM1.RoW>=0.05])
setdiff(MSE.forecastE$Rep[MSE.forecastE$DM1.ReW>=0.05&MSE.forecastE$DM1.RoW>=0.05],MSE.forecastI$Rep[MSE.forecastI$DM1.ReW>=0.05&MSE.forecastI$DM1.RoW>=0.05])

intersect(MSE.forecastE$Rep[MSE.forecastE$DM1.RoW<=0.05 & MSE.forecastE$RoWSup1<=0.05],MSE.forecastI$Rep[MSE.forecastI$DM1.RoW<=0.05 & MSE.forecastI$RoWSup1<=0.05])

union(MSE.forecastE$Rep[MSE.forecastE$DM1.ReW>=0.05&MSE.forecastE$DM1.RoW>=0.05],MSE.forecastI$Rep[MSE.forecastI$DM1.ReW>=0.05&MSE.forecastI$DM1.RoW>=0.05])

setdiff(MSE.forecast$Rep[MSE.forecast$DM3.RoW<=0.05],MSE.forecast$Rep[MSE.forecast$DM1.RoW<=0.05])
setdiff(MSE.forecast$Rep[MSE.forecast$DM3.ReW<=0.05],MSE.forecast$Rep[MSE.forecast$DM1.ReW<=0.05])
intersect(setdiff(MSE.forecast$Rep[MSE.forecast$DM3.RoW<=0.05],MSE.forecast$Rep[MSE.forecast$DM1.RoW<=0.05]),setdiff(MSE.forecast$Rep[MSE.forecast$DM3.ReW<=0.05],MSE.forecast$Rep[MSE.forecast$DM1.ReW<=0.05]))
MSE.forecast$Rep[MSE.forecast$DM1.RoW<=0.05]

MSE.forecast$Rep[MSE.forecast$MSE1.ReW<=MSE.forecast$MSE1.random & MSE.forecast$DM1.ReW<=0.05] #ReW d
MSE.forecast$Rep[MSE.forecast$MSE1.RoW>=MSE.forecast$MSE1.random & MSE.forecast$DM1.RoW<=0.05]

MSE.forecast$Rep[MSE.forecast$MSE1.RoW<=MSE.forecast$MSE1.random & MSE.forecast$DM1.RoW<=0.05]

