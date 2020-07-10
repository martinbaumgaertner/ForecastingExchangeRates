library(pacman)
p_load(readxl,readr,ggfortify,reshape,ggplot2,vars,grid,gridExtra,zoo,pracma,lubridate,dplyr,tidyr,xtable,cowplot,dyn,tidyverse, gridExtra,
       reshape2, scales,readxl,stargazer,dynlm,AER,RColorBrewer,
       install = TRUE, update = F)

simple_delpic<-T
altavilla_delphic<-F
qe_start<-"2014-10-01"

merge_extended<-function(x,y,b){
  inter <- intersect(names(x), names(y))
  inter=inter[inter!=b]
  diffx <- setdiff(names(x),inter)
  diffy <- setdiff(names(y),inter)
  
  different_merge<-merge(x[,c(diffx)],y[,c(diffy)],all=T)
  
  min_d<-c(head(x[,b],n=1),head(y[,b],n=1))
  max_d<-c(tail(x[,b],n=1),tail(y[,b],n=1))
  
  
  date_full<-data.frame(Date=seq(min_d[which.min(min_d)],max_d[which.max(max_d)],by="days"))
  inter_merge<-data.frame(Date=date_full)
  
  for (i in inter){
    
    dd<-merge(x[,c(b,i)],y[,c(b,i)],by=b,all=TRUE)
    dd[complete.cases(dd),3]<-NA
    
    x_temp<-data.frame(na.omit(dd[,c(1,2)]));colnames(x_temp)<-c("Date",i)
    y_temp<-data.frame(na.omit(dd[,c(1,3)]));colnames(y_temp)<-c("Date",i)
    s<-merge(y_temp,x_temp,all=T)
    
    inter_merge<-merge(inter_merge,s,all=T)
  }
  full_merge<-merge(different_merge,inter_merge,by=b,all = T)
  return(full_merge)
}
classify<-function(hfd,factors,simple=T,altavilla=F,window="mew"){
  if(altavilla==T){
    if(window=="conference"){
      factors=factors[complete.cases(factors),]
      variable_pre<-paste("Timing+FG")
      variable_post<-paste("Timing+FG+QE")
    }else if (window=="release"){
      variable_pre<-paste("Conventional")
      variable_post<-paste("Conventional")
    }else if (window=="mew"){
      factors=factors[complete.cases(factors),]
      variable_pre<-paste("Conventional+Timing+FG")
      variable_post<-paste("Conventional+Timing+FG+QE")
    }else{
      print("Please provide information about the time-window")
    }
    
    hfd<-merge(hfd,factors,by="date")
    
    
    if(simple==T){
      
      mp_pre <- fitted(lm(as.formula(paste("OIS_2Y_d ~ ",paste(variable_pre),sep = "")), data = hfd %>% filter(date<ymd("2008-01-01"))))
      mp_btw <- fitted(lm(as.formula(paste("OIS_2Y_d ~ ",paste(variable_pre),sep = "")), data = hfd %>% filter(date<qe_start, date>="2008-01-01")))
      mp_pst <- fitted(lm(as.formula(paste("OIS_2Y_d ~ ",paste(variable_post),sep = "")), data = hfd %>% filter(date>=qe_start)))
      
      sto_pre <- fitted(lm(as.formula(paste("STOXX50_d ~ ",paste(variable_pre),sep = "")), data = hfd %>% filter(date<"2008-01-01")))
      sto_btw <- fitted(lm(as.formula(paste("STOXX50_d ~ ",paste(variable_pre),sep = "")), data = hfd %>% filter(date<qe_start, date>="2008-01-01")))
      sto_pst <- fitted(lm(as.formula(paste("STOXX50_d ~ ",paste(variable_post),sep = "")), data = hfd %>% filter(date>=qe_start)))
      
      mp <- c(mp_pre, mp_btw, mp_pst)
      sto <- c(sto_pre, sto_btw, sto_pst)
      
      hfd <- hfd %>% mutate(OIS_2Y_d = mp, 
                            STOXX50_d = sto)
      
      hfd <- hfd %>% dplyr::select(STOXX50_d,OIS_2Y_d,date) %>%
        mutate(mptype = case_when(
          (sign(OIS_2Y_d) * sign(STOXX50_d) == -1)  ~ "monetary",
          sign(OIS_2Y_d) * sign(STOXX50_d) == 1 ~ "information",
          TRUE ~ "Other"))
    }else{
      
      mp_pre <- c(NA,fitted(lm(as.formula(paste("OIS_2Y_d ~ ",paste(variable_pre),sep = "")), data = hfd %>% filter(date<ymd("2008-01-01")))))
      mp_btw <- fitted(lm(as.formula(paste("OIS_2Y_d ~ ",paste(variable_pre),sep = "")), data = hfd %>% filter(date<qe_start, date>="2008-01-01")))
      mp_pst <- fitted(lm(as.formula(paste("OIS_2Y_d ~ ",paste(variable_post),sep = "")), data = hfd %>% filter(date>=qe_start)))
      
      ils_pre <- c(rep(NA, 36),fitted(lm(as.formula(paste("ILS2Y_d ~ ",paste(variable_pre),sep = "")), data = hfd %>% filter(date<"2008-01-01"))))
      ils_btw <- fitted(lm(as.formula(paste("ILS2Y_d ~ ",paste(variable_pre),sep = "")), data = hfd %>% filter(date<qe_start, date>="2008-01-01")))
      ils_pst <- fitted(lm(as.formula(paste("ILS2Y_d ~ ",paste(variable_post),sep = "")), data = hfd %>% filter(date>=qe_start)))
      
      sto_pre <- fitted(lm(as.formula(paste("STOXX50_d ~ ",paste(variable_pre),sep = "")), data = hfd %>% filter(date<"2008-01-01")))
      sto_btw <- fitted(lm(as.formula(paste("STOXX50_d ~ ",paste(variable_pre),sep = "")), data = hfd %>% filter(date<qe_start, date>="2008-01-01")))
      sto_pst <- fitted(lm(as.formula(paste("STOXX50_d ~ ",paste(variable_post),sep = "")), data = hfd %>% filter(date>=qe_start)))
      
      mp <- c(mp_pre, mp_btw, mp_pst)
      ils <- c(ils_pre, ils_btw, ils_pst)
      sto <- c(sto_pre, sto_btw, sto_pst)
      
      hfd <- hfd %>% mutate(OIS_2Y_d = mp, 
                            ILS2Y_d = ils, 
                            STOXX50_d = sto)
      
      hfd <- hfd %>% dplyr::select(STOXX50_d,OIS_2Y_d,ILS2Y_d,date) %>%
        mutate(mptype = case_when(
          (sign(OIS_2Y_d) * sign(STOXX50_d) == -1) & (sign(ILS2Y_d) == sign(STOXX50_d))  ~ "monetary",
          abs(sign(OIS_2Y_d) + sign(STOXX50_d) + sign(ILS2Y_d)) == 3 ~ "information",
          TRUE ~ "Other"
        ))
      hfd[!complete.cases(hfd),"mptype"]<-NA
    }
  }else{
    if(simple==T){
      hfd <- hfd %>%dplyr::select(STOXX50_d,OIS_2Y_d,date) %>%
        mutate(mptype = case_when(
          (sign(OIS_2Y_d) * sign(STOXX50_d) == -1) ~ "monetary",
          (sign(OIS_2Y_d) * sign(STOXX50_d) == 1) ~ "information",
          TRUE ~ "Other"))
    }else{
      hfd <- hfd %>% dplyr::select(STOXX50_d,ILS2Y_d,OIS_2Y_d,date) %>%
        mutate(mptype = case_when(
          (sign(OIS_2Y_d) * sign(STOXX50_d) == -1) & (sign(ILS2Y_d) == sign(STOXX50_d))  ~ "monetary",
          abs(sign(OIS_2Y_d) + sign(STOXX50_d) + sign(ILS2Y_d)) == 3 ~ "information",
          TRUE ~ "Other"))
      hfd[!complete.cases(hfd),"mptype"]<-NA
    }
  }
  
  hfd<-hfd %>% 
    dplyr::select("date","mptype")
  
  if(altavilla==T){
    if(simple==T){
      colnames(hfd)[colnames(hfd)=="mptype"]<-paste("regression",window,2,sep="_")
    }else{
      colnames(hfd)[colnames(hfd)=="mptype"]<-paste("regression",window,3,sep="_")
    }
  }else{
    if(simple==T){
      colnames(hfd)[colnames(hfd)=="mptype"]<-paste("poorman",window,2,sep="_")
    }else{
      colnames(hfd)[colnames(hfd)=="mptype"]<-paste("poorman",window,3,sep="_")
    }
  }
  
  return(as_tibble(hfd))
}
hfd_classify<-function(data,classif,column){
  d<-select(data,c("date",column)) %>% 
    setNames(c("date","variable"))
  d1<- classif %>% 
    setNames(c("date","classification"))
  d2<-left_join(d,d1,by="date") %>% 
    mutate(!!paste0(column,"_odyss"):=ifelse(classification=="monetary",variable,0)) %>% 
    mutate(!!paste0(column,"_delphic"):=ifelse(classification=="information",variable,0)) %>% 
    select(-variable,-classification)
  return(left_join(data,d2,by="date"))
}
roll<-function(data){
  return(rollapply(data,width=31,FUN=sum, na.rm = TRUE,partial=TRUE,align="right", by.column = TRUE))
}

altavilla<-read_csv("data/altavilla/ois_factors.csv", col_types=cols(date = col_datetime("%d.%m.%Y"))) %>% 
  replace(., is.na(.), 0) %>% 
  rename(Conventional=RateFactor1,
         Timing=ConfFactor1,
         FG=ConfFactor2,
         QE=ConfFactor3) %>% 
  mutate(QE = ifelse(date <qe_start, 0, QE))%>% 
  mutate(Broad_FG=rowSums(select(.,Timing,FG)),
         Unconventional=rowSums(select(.,Timing,FG,QE)),
         Total=rowSums(select(.,Conventional,Timing,FG,QE))) 

coltypes<-cols(.default = col_double(),date = col_datetime(format = ""))
HFD_mew<-read_csv("data/altavilla/altavilla_mew.csv", col_types = coltypes)
HFD_release<-read_csv("data/altavilla/altavilla_prw.csv", col_types = coltypes)
HFD_conference<-read_csv("data/altavilla/altavilla_pcw.csv", col_types = coltypes)

classification_conference<-classify(HFD_conference,altavilla,window="conference",simple=T,altavilla = F)
classification_release<-classify(HFD_release,altavilla,window="release",simple=T,altavilla = F)
classification_mew<-classify(HFD_mew,altavilla,window="mew",simple=T,altavilla = F)

odys<-altavilla %>% 
  hfd_classify(classification_release,"Conventional") %>% 
  hfd_classify(classification_conference,"Timing") %>% 
  hfd_classify(classification_conference,"FG") %>% 
  hfd_classify(classification_conference,"QE") %>% 
  hfd_classify(classification_conference,"Broad_FG") %>% 
  hfd_classify(classification_conference,"Unconventional") %>% 
  hfd_classify(classification_mew,"Total") 

Date_cum<-tibble(date=seq.POSIXt(floor_date(min(odys$date),"month"),max(odys$date),by="days")) 
mean_shocks<-left_join(Date_cum,odys,by="date") %>%
  mutate_at(colnames(.)[-1],roll)%>% 
  group_by(date=floor_date(date, "month")) %>%
  summarize_at(colnames(.)[-c(1)],mean) %>% 
  mutate_at(colnames(.)[-1],~./100)

monthly<-read_csv("data/data_monthly.csv", col_types = coltypes)
weekly<-read_csv("data/data_weeklymonthly.csv", col_types = coltypes)
daily<-read_csv("data/data_dailymonthly.csv", col_types = coltypes)
data<-full_join(monthly,weekly,by="date") %>% 
  full_join(daily,by="date") %>% 
  full_join(mean_shocks,by="date")%>% 
  filter(date>="2002-01-01")

save.image(file='data/data_processed.RData')
