library(pacman)
p_load(readxl,readr,ggfortify,reshape,ggplot2,vars,grid,gridExtra,zoo,pracma,lubridate,dplyr,tidyr,xtable,cowplot,dyn,tidyverse, gridExtra,
       reshape2, scales,readxl,stargazer,dynlm,AER,RColorBrewer,dynlm,rlist,patchwork,
       install = TRUE, update = F)

load('data/data_processed.RData')

sh<-c("Conventional","Unconventional","Timing","Forward Guidance","QE","Total","Odyssean","Delphic")
sh.col<-brewer.pal(9,"Set1")
sh.col[sh.col=="#F781BF"]<-"#37a603"
sh.col[sh.col=="#FFFF33"]<-"#5203a6"
sh.col[sh.col=="#A65628"]<-"#e3b017"
names(sh.col)<-sh

##-------descriptive----
shocks<-data[,c("date","Conventional","Unconventional")]
colnames(shocks)[colnames(shocks)=="FG"]<-"Forward Guidance"
shocks<-melt(shocks,id.vars = "date",variable_name = "Factor")
colnames(shocks)[2]<-"Factor"
two_shocks<-ggplot(shocks, aes(x = date, y = value, fill = Factor)) + 
  theme_bw()+
  xlab("")+
  ylab("")+
  geom_col(position="stack")+ 
  scale_fill_manual(values = sh.col)

shocks<-data[,c("date","Conventional","Timing","FG","QE")]
colnames(shocks)[colnames(shocks)=="FG"]<-"Forward Guidance"
shocks<-melt(shocks,id.vars = "date",variable_name = "Factor")
colnames(shocks)[2]<-"Surprise"
single_shocks<-ggplot(shocks, aes(x = date, y = value, fill = Surprise)) + 
  theme_bw()+
  xlab("")+
  ylab("")+
  geom_col(position="stack")+ 
  scale_fill_manual(values = sh.col)

ggsave2("Graphics/single_shocks.png",single_shocks, height=h, width=w, units='in', dpi=dpi)
Factors_combined<-plot_grid(plotlist = list(two_shocks,single_shocks), ncol = 1, align = "v")
ggsave2("Graphics/Factors_combined.png",Factors_combined, height=h, width=w, units='in', dpi=dpi)

rm(list=setdiff(ls(), c("data","qe_start","sh.col")))

##-------VAR settings----
get_data<-function(data1,E,Inst,pri=NULL,border=NULL){
  if(!is.null(border)){
    data1<-subset(data1,date<=border)
  }
  
  Endo<-data1[,c("date",E)]
  Endo<-Endo[complete.cases(Endo),]
  emin<<-min(Endo$date)
  emax<<-max(Endo$date)
  IV<-data1[,c("date",Inst)]
  IV_com<-IV[complete.cases(IV),]
  Instmin<<-min(IV_com$date)
  Instmax<<-max(IV_com$date)
  
  IV<<-merge(Endo,IV,by="date")[,dim(Endo)[2]+1]
  Endo<<-Endo[,-1]
  
  if(!is.null(pri)){
    cat(sprintf("Endo Range: %s - %s\n", paste(emin),paste(emax)))
    cat(sprintf("Instrument Range: %s - %s \n", paste(Instmin),paste(Instmax)))
  }
}
pretty.irf<-function(irf1){
  impulse.all<-irf1$impulse
  responses.all<-irf1$response
  mean.all<-data.frame(irf1$irf)
  lower.all<-data.frame(irf1$Lower)
  upper.all<-data.frame(irf1$Upper)
  n.ahead<-length(mean.all[,1])
  lags<-c(1:n.ahead)
  n<-length(impulse.all)
  b<-length(responses.all)
  plots <- array(list(), dim = c(b, n))
  scaleFUN <- function(x) sprintf("%.3f", x)
  for (i in 1:n){#shock
    for (j in 1:b){#response
      mean<-mean.all[,paste(impulse.all[i],".",responses.all[j],sep="")]
      lower<-lower.all[,paste(impulse.all[i],".",responses.all[j],sep="")]
      upper<-upper.all[,paste(impulse.all[i],".",responses.all[j],sep="")]
      impulse=impulse.all[i]
      response=responses.all[j]
      data<-data.frame(lags,mean,lower,upper)
      data<-round(data,4)
      plots[[j,i]]<-ggplot(data)+
        geom_ribbon(aes(lags,ymin=upper,ymax=lower),fill='#80ba24',alpha=0.2)+
        geom_line(aes(lags,mean),linetype="solid",color='#4a5c66',size=1.2)+
        geom_line(aes(lags,upper),linetype="dashed",color='#80ba24',size=1)+
        geom_line(aes(lags,lower),linetype="dashed",color='#80ba24',size=1)+
        geom_hline(yintercept=0)+
        ylab(response)+
        xlab("")+
        theme_bw()+
        theme(plot.margin = unit(c(0,5,0,5), "mm"))+
        #scale_x_continuous(breaks=number_ticks(10))+
        scale_y_continuous(labels=scaleFUN)
      if(j!=b){ plots[[j,i]]<-plots[[j,i]]+
        ylab(response)+
        theme(axis.text.x=element_blank(),
              axis.ticks=element_blank())
      }
      if(j==1){plots[[j,i]]<-plots[[j,i]]+
        ggtitle(paste("Shock in",impulse))+
        theme(plot.title=element_text(size=10, vjust=-1,hjust = 0.5))
      }  
      if(i!=1){plots[[j,i]]<-plots[[j,i]]+
        ylab("")
      }
    }
  }
  cowplot::plot_grid(plotlist = plots, ncol = n, align = "v")
}
get_legend<-function(myggplot){
      tmp <- ggplot_gtable(ggplot_build(myggplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)
}
#note the suppressed warnings! result is right
pretty.irf_complete<-function(data1,names1,leg=0,response="point"){
number_irf<-length(data1)
lags<-seq(1,dim(data1[[1]]$irf)[1])
b<-dim(data1[[1]]$irf)[2]
n=1
names<-colnames(data1[[1]]$irf)
if(length(intersect(names,"Industry_exc_const_ls"))!=0){
  names[names==c("Industry_exc_const_ls")]<-c("Output")
}
if(length(intersect(names,"HICP_ls"))!=0){
  names[names==c("HICP_ls")]<-c("Prices")
}
if(length(intersect(names,"VIX_new"))!=0){
  names[names==c("VIX_new")]<-c("VIX")
}
if(length(intersect(names,"commodity"))!=0){
  names[names==c("commodity")]<-c("Commodity")
}
if(length(intersect(names,"Stoxx50_ls"))!=0){
  names[names==c("Stoxx50_ls")]<-c("Stocks")
}
if(length(intersect(names,"ciss"))!=0){
  names[names==c("ciss")]<-c("Uncertainty")
}

plots <- array(list(), dim = c(b, n))
scaleFUN <- function(x) sprintf("%.3f", x)
if (response=="mean"){
  dat<-array(list(), dim = c(b, n))
  for (j in 1:b){#response
    for(a in 1:number_irf){#number of irfs
        if (a==1){
        irf<-tibble(lags,mean=data1[[a]]$irf[,j],
                        lower=data1[[a]]$Lower[,j],
                        upper=data1[[a]]$Upper[,j],
                        Factor=rep(names1[a],length(lags)),
                        macro=rep(names[j],length(lags))
                        )
        irf_full<-irf
        }else{
          irf<-tibble(lags,mean=data1[[a]]$irf[,j],
                          lower=data1[[a]]$Lower[,j],
                          upper=data1[[a]]$Upper[,j],
                          Factor=rep(names1[a],length(lags)),
                          macro=rep(names[j],length(lags)))
          irf_full<-suppressWarnings(merge(irf_full,irf,by=c("lags","mean","lower","upper","Factor","macro"),all=T))
        }
      dat[[j]]<-irf_full
    }
  }
}else if (response=="median"){
  dat<-array(list(), dim = c(b, n))
  for (j in 1:b){#response
    for(a in 1:number_irf){#number of irfs
      if (a==1){
        irf<-data.frame(lags,mean=data1[[a]]$Median[,j],
                        lower=data1[[a]]$Lower[,j],
                        upper=data1[[a]]$Upper[,j],
                        Factor=rep(names1[a],length(lags)),
                        macro=rep(names[j],length(lags))
        )
        irf_full<-irf
      }else{
        irf<-data.frame(lags,mean=data1[[a]]$Median[,j],
                        lower=data1[[a]]$Lower[,j],
                        upper=data1[[a]]$Upper[,j],
                        Factor=rep(names1[a],length(lags)),
                        macro=rep(names[j],length(lags)))
        irf_full<-suppressWarnings(merge(irf_full,irf,by=c("lags","mean","lower","upper","Factor","macro"),all=T))
      }
      dat[[j]]<-irf_full
    }
  }  
}else if (response=="point"){
  dat<-array(list(), dim = c(b, n))
  for (j in 1:b){#response
    for(a in 1:number_irf){#number of irfs
      if (a==1){
        irf<-data.frame(lags,mean=data1[[a]]$point[,j],
                        lower=data1[[a]]$Lower[,j],
                        upper=data1[[a]]$Upper[,j],
                        Factor=rep(names1[a],length(lags)),
                        macro=rep(names[j],length(lags))
        )
        irf_full<-irf
      }else{
        irf<-data.frame(lags,mean=data1[[a]]$point[,j],
                        lower=data1[[a]]$Lower[,j],
                        upper=data1[[a]]$Upper[,j],
                        Factor=rep(names1[a],length(lags)),
                        macro=rep(names[j],length(lags)))
        irf_full<-suppressWarnings(merge(irf_full,irf,by=c("lags","mean","lower","upper","Factor","macro"),all=T))
      }
      dat[[j]]<-irf_full
    }
  }  
}

for (i in 1:n){#shock
  for (j in 1:b){#response
    da<-dat[[j]]
    da[-c(1,5,6)]<-da[-c(1,5,6)]*-1
    da[-c(1,5,6)]<-round(da[-c(1,5,6)],4)
    plots[[j,i]]<-ggplot(data=da)+
      geom_line(aes(lags,mean,group=Factor,color=Factor),size=1.2)+
      geom_ribbon(aes(lags,ymin=lower,ymax=upper,group=Factor,fill=Factor),alpha=0.2)+ 
      geom_hline(yintercept=0)+
      ylab(names[j])+
      xlab("")+
      theme_bw()+
      theme(plot.margin = unit(c(0,5,0,5), "mm"))+
      #scale_x_continuous(breaks=number_ticks(10))+
      scale_y_continuous(labels=scaleFUN)+
      scale_fill_manual(values = sh.col)+
      scale_color_manual(values = sh.col)
    if(j!=b){ plots[[j,i]]<-plots[[j,i]]+
      ylab(names[j])+
      theme(axis.text.x=element_blank(),
            axis.ticks=element_blank())
    }
    if(j==1){plots[[j,i]]<-plots[[j,i]]+
      ggtitle("Expansionary monetary policy shock")+
      theme(plot.title=element_text(size=10, vjust=-1,hjust = 0.5))
    }
  }
}
legend <- get_legend(plots[[1,1]])
for (i in 1:n){#shock
  for (j in 1:b){#response
    plots[[j,i]]<-plots[[j,i]]+ theme(legend.position="none")
  }
}
if(leg==1){
  t<-cowplot::plot_grid(plotlist = plots, ncol = n, align = "v")
  cowplot::plot_grid(t,legend, ncol = 2, rel_widths = c(3, .4))
}else{
  cowplot::plot_grid(plotlist = plots, ncol = n, align = "v")
}
}
column_values<-function(x){
  return(ggplot_build(x)$layout$panel_params[[1]]$y.range)
}
pretty.irf_sbs<-function(data1,names1,leg=0,response="point",same_scale=T){
number_irf<-length(data1)
lags<-seq(1,dim(data1[[1]]$irf)[1])
b<-dim(data1[[1]]$irf)[2]
n=length(data1)
names<-colnames(data1[[1]]$irf)

if(length(intersect(names,"Industry_exc_const_ls"))!=0){
  names[names==c("Industry_exc_const_ls")]<-c("Output")
}
if(length(intersect(names,"industry_sl"))!=0){
  names[names==c("industry_sl")]<-c("Output")
}
if(length(intersect(names,"HICP_ls"))!=0){
  names[names==c("HICP_ls")]<-c("Prices")
}
if(length(intersect(names,"HICP_sl"))!=0){
  names[names==c("HICP_sl")]<-c("Prices")
}
if(length(intersect(names,"VIX_new"))!=0){
  names[names==c("VIX_new")]<-c("VIX")
}
if(length(intersect(names,"commodity"))!=0){
  names[names==c("commodity")]<-c("Commodity")
}
if(length(intersect(names,"PALLFNF_sl"))!=0){
  names[names==c("PALLFNF_sl")]<-c("Commodity")
}
if(length(intersect(names,"Stoxx50_ls"))!=0){
  names[names==c("Stoxx50_ls")]<-c("Stocks")
}
if(length(intersect(names,"Stoxx50_sl"))!=0){
  names[names==c("Stoxx50_sl")]<-c("Stocks")
}
if(length(intersect(names,"ciss"))!=0){
  names[names==c("ciss")]<-c("Uncertainty")
}
plots <- array(list(), dim = c(b, n))
scaleFUN <- function(x) sprintf("%.3f", x)
dat<-array(list(), dim = c(b, n))
if (response=="mean"){
  for(a in 1:n){#number of irfs
    for (j in 1:b){#response
        irf<-tibble(lags,mean=data1[[a]]$irf[,j],
                        lower=data1[[a]]$Lower[,j],
                        upper=data1[[a]]$Upper[,j],
                        Factor=rep(names1[a],length(lags)),
                        macro=rep(names[j],length(lags))
                        )
        irf_full<-irf
      dat[[j,a]]<-irf_full
    }
  }
}else if (response=="median"){
  for(a in 1:number_irf){#number of irfs
    for (j in 1:b){#response
      irf<-tibble(lags,mean=data1[[a]]$Median[,j],
                  lower=data1[[a]]$Lower[,j],
                  upper=data1[[a]]$Upper[,j],
                  Factor=rep(names1[a],length(lags)),
                  macro=rep(names[j],length(lags))
      )
      irf_full<-irf
      dat[[j,a]]<-irf_full
    }
  }
}else if (response=="point"){
  for(a in 1:number_irf){#number of irfs
    for (j in 1:b){#response
      irf<-tibble(lags,mean=data1[[a]]$point[,j],
                  lower=data1[[a]]$Lower[,j],
                  upper=data1[[a]]$Upper[,j],
                  Factor=rep(names1[a],length(lags)),
                  macro=rep(names[j],length(lags))
      )
      irf_full<-irf
      dat[[j,a]]<-irf_full
    }
  }
}
for (i in 1:n){#shock
  for (j in 1:b){#response
    da<-dat[[j,i]]
    da[-c(1,5,6)]<-da[-c(1,5,6)]*-1
    da[-c(1,5,6)]<-round(da[-c(1,5,6)],4)
    plots[[j,i]]<-ggplot(data=da)+
      geom_line(aes(lags,mean,group=Factor,color=Factor),size=1.2)+
      geom_ribbon(aes(lags,ymin=lower,ymax=upper,group=Factor,fill=Factor),alpha=0.2)+ 
      geom_hline(yintercept=0)+
      ylab(names[j])+
      xlab("")+
      theme_bw()+
      theme(plot.margin = unit(c(0,5,0,5), "mm"))+
      #scale_x_continuous(breaks=number_ticks(10))+
      scale_y_continuous(labels=scaleFUN)+
      scale_fill_manual(values = sh.col)+
      scale_color_manual(values = sh.col)
    if(j!=b){ plots[[j,i]]<-plots[[j,i]]+
      ylab(names[j])+
      theme(axis.text.x=element_blank(),
            axis.ticks=element_blank())
    }
    if(j==1){plots[[j,i]]<-plots[[j,i]]+
      ggtitle(paste(unique(da$Factor),"shock"))+
      theme(plot.title=element_text(size=10, vjust=-1,hjust = 0.5))
    }
  }
}

legend <- get_legend(plots[[1,1]])
for (i in 1:n){#shock
  for (j in 1:b){#response
    plots[[j,i]]<-plots[[j,i]]+ theme(legend.position="none")
  }
}
if(same_scale==T){
  for(j in 1:n){
    for(i in 1:nrow(plots)){
      maxi<-max(unlist(lapply(plots[i,], column_values)))
      mini<-min(unlist(lapply(plots[i,], column_values)))
      
      plots[[i,j]]<-plots[[i,j]]+
        coord_cartesian( ylim=c(mini, maxi))
      if(j!=1){
        plots[[i,j]]<-plots[[i,j]] +
          theme(axis.title.y=element_blank(),
                axis.text.y=element_blank())
      }
    }
  }
}
wrap_plots(plots,ncol=n,guides="collect",byrow=F)
}
#external instrument var functions adapted code from Ambrogio Cesa Bianchi (2015)
CommonSample<-function(DATA){
  dim=1
  fo=0
  lo=0
  
  if(dim==1){
    temp=rowSums(DATA)
    ii=1
    if(!is.na(temp[ii])){
      while (!is.nan(temp[ii])) {
        fo=fo+1
        ii=ii+1
        if(ii>length(temp)){
          break
        }
        
      }
    }
    for(ii in 1:(dim(DATA)[1]-fo)){
      if(is.na(temp[length(temp)+1-ii])){
        lo=lo+1
      }
    }
    DATA<-DATA[complete.cases(DATA),]
  }
  out<-list(DATA,fo,lo)
  return(out)
}
var_inst<-function(Endo,IV){
  out<-list()
  var_data<-VAR(Endo,type=var_type,p=var_lag_order);vars::roots(var_data)
  
  ext<-dim(Endo)[2]
  up<-residuals(var_data)[,ext]
  uq<-residuals(var_data)[,-ext]
  
  endo_common<-data.frame(date=seq(as.Date(emin)%m+% months(var_lag_order),as.Date(emax),by="month"),up)
  ivcommon<-data.frame(date=seq(as.Date(Instmin)%m+% months(var_lag_order),as.Date(emax),by="month"),IV[(var_lag_order+1):length(IV)])
  
  #redf.Y<-var_data$y[-c(1:var_data$p),]
  #redf.X<-data.matrix(var_data$datamat)[,-c(1:5)]
  #redf.X<-cbind(redf.X[,dim(redf.X)[2]],redf.X[,-dim(redf.X)[2]])
  #slopeparameters<-Bcoef(var_data)
  #slopeparameters<-cbind(slopeparameters[,dim(slopeparameters)[2]],slopeparameters[,-dim(slopeparameters)[2]])
  #redf.AL<-slopeparameters[,-1]
  #redf.nu<-slopeparameters[,1]
  #redf.eta<-redf.Y-redf.X%*%t(slopeparameters) # or just residuals(var_data).... eta=up+uq
  #redf.Psi<-(t(redf.eta)%*%redf.eta)/dim(redf.eta)[1]
  
  #redf.gamma<-t(redf.eta)%*%Z/dim(redf.eta)[1]
  
  #redf.beta<-as.numeric(coefficients(lm(Z~0+redf.X)))
  #v_sample<-Z-redf.X%*%redf.beta
  
  #d = length(redf.gamma)
  #Psiinv_gamma<-coefficients(lm(redf.gamma~0+redf.Psi))
  #denom = sqrt(t(redf.gamma)%*%Psiinv_gamma)
  #H = suppressWarnings(apply(redf.gamma, 2, function(data, ind){
  #  div<-data/ind
  #  return(div)}, denom)) 
  
  #beta[length(beta)]<-beta[1]
  #beta<-beta[-1]
  
  
  common<-merge(endo_common,ivcommon,by="date",all=TRUE)
  colnames(common)<-c("endo_common","ivcommon")
  #comparable sample?
  d<-CommonSample(common[,-1])
  aux<-d[[1]]
  
  p<-aux[,1]
  q<-uq[(dim(uq)[1]-length(p)+1):dim(uq)[1],]
  Z<-aux[,2]
  
  OLS<-lm(p~Z)
  out$r2<-summary(OLS)$r.squared
  out$r.summary<-coeftest(OLS, vcov = vcovHAC(OLS))
  out$f.robust<-waldtest(OLS, vcov = vcovHAC(OLS))$F[2]
  
  p_hat<-OLS$fitted.values
  
  IRF<-rep(NA,var_data$K)
  IRF[ext]<-1
    for(i in 1:(var_data$K-1)){
    second<-lm(q[,i]~0+p_hat)
    IRF[i]<-second$coefficients
  }
  

  
  K <- var_data$K
  p <- var_data$p
  A <- unlist(Acoef(var_data))
  F1 <- matrix(0, nrow = K * p, ncol = K * p) #companion form
  F1[1:K, 1:(K * p)] <- A
  if (p > 1) {
    j <- 0
    for (i in (K + 1):(K * p)) {
      j <- j + 1
      F1[i, j] <- 1
    }
  }
  
  IRF<-c(IRF,rep(0,dim(F1)[1]-length(IRF)))
  
  IRF_big<-matrix(NA,dim(F1)[1],irf_steps)
  IRF_big[,1]<-IRF
  
  for (i in 2:irf_steps){
    IRF_big[,i]<-F1%*%IRF_big[,i-1]
  }
  IRF=t(IRF_big[(1:(ext)),])
  
  ols_sum<-summary(OLS)$fstatistic
  
  out$f<-ols_sum[1]
  out$f_pvalue<-pf(ols_sum[1],ols_sum[2],ols_sum[3],lower.tail=FALSE)
  out$r2<-summary(OLS)$r.squared 
  out$first<-OLS
  out$fitted<-p
  out$IRF<-IRF
  out$basevar<-var_data
  return(out)
}
irf_boot<-function(var_data,point){
  nvar    <- var_data$K
  nlag    <- var_lag_order
  nobs    <- var_data$obs
  resid   <- residuals(var_data)
  ENDO    <- var_data$y
  ext<-dim(ENDO)[2]
  method <-"wild"
  nsteps<-irf_steps
  ndraws <-irf_runs
  pctg   <-irf_conf
  names<-colnames(var_data$y)
  #method = VARopt.method;
  if(var_data$type=="const"){
    const=1
    if(ext==3){
      Ft<-cbind(c(coef(var_data)[[names[1]]][nlag*nvar+const,1],coef(var_data)[[names[1]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[2]]][nlag*nvar+const,1],coef(var_data)[[names[2]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[3]]][nlag*nvar+const,1],coef(var_data)[[names[3]]][-c(nlag*nvar+const),1]))
    } else if(ext==4){
      Ft<-cbind(c(coef(var_data)[[names[1]]][nlag*nvar+const,1],coef(var_data)[[names[1]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[2]]][nlag*nvar+const,1],coef(var_data)[[names[2]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[3]]][nlag*nvar+const,1],coef(var_data)[[names[3]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[4]]][nlag*nvar+const,1],coef(var_data)[[names[4]]][-c(nlag*nvar+const),1]))
    }else if(ext==5){
      Ft<-cbind(c(coef(var_data)[[names[1]]][nlag*nvar+const,1],coef(var_data)[[names[1]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[2]]][nlag*nvar+const,1],coef(var_data)[[names[2]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[3]]][nlag*nvar+const,1],coef(var_data)[[names[3]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[4]]][nlag*nvar+const,1],coef(var_data)[[names[4]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[5]]][nlag*nvar+const,1],coef(var_data)[[names[5]]][-c(nlag*nvar+const),1]))
    }else if(ext==6){
      Ft<-cbind(c(coef(var_data)[[names[1]]][nlag*nvar+const,1],coef(var_data)[[names[1]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[2]]][nlag*nvar+const,1],coef(var_data)[[names[2]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[3]]][nlag*nvar+const,1],coef(var_data)[[names[3]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[4]]][nlag*nvar+const,1],coef(var_data)[[names[4]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[5]]][nlag*nvar+const,1],coef(var_data)[[names[5]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[6]]][nlag*nvar+const,1],coef(var_data)[[names[6]]][-c(nlag*nvar+const),1]))
    }else if(ext==7){
      Ft<-cbind(c(coef(var_data)[[names[1]]][nlag*nvar+const,1],coef(var_data)[[names[1]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[2]]][nlag*nvar+const,1],coef(var_data)[[names[2]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[3]]][nlag*nvar+const,1],coef(var_data)[[names[3]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[4]]][nlag*nvar+const,1],coef(var_data)[[names[4]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[5]]][nlag*nvar+const,1],coef(var_data)[[names[5]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[6]]][nlag*nvar+const,1],coef(var_data)[[names[6]]][-c(nlag*nvar+const),1]),
                c(coef(var_data)[[names[7]]][nlag*nvar+const,1],coef(var_data)[[names[7]]][-c(nlag*nvar+const),1]))
    }
  }else{
    const=0
    if(ext==3){
      Ft<-cbind(coef(var_data)[[names[1]]][,1],
                coef(var_data)[[names[2]]][,1],
                coef(var_data)[[names[3]]][,1])
    } else if(ext==4){
      Ft<-cbind(coef(var_data)[[names[1]]][,1],
                coef(var_data)[[names[2]]][,1],
                coef(var_data)[[names[3]]][,1],
                coef(var_data)[[names[4]]][,1])
    }else if(ext==5){
      Ft<-cbind(coef(var_data)[[names[1]]][,1],
                coef(var_data)[[names[2]]][,1],
                coef(var_data)[[names[3]]][,1],
                coef(var_data)[[names[4]]][,1],
                coef(var_data)[[names[5]]][,1])
    }else if(ext==6){
      Ft<-cbind(coef(var_data)[[names[1]]][,1],
                coef(var_data)[[names[2]]][,1],
                coef(var_data)[[names[3]]][,1],
                coef(var_data)[[names[4]]][,1],
                coef(var_data)[[names[5]]][,1],
                coef(var_data)[[names[6]]][,1])
    }else if(ext==7){
      Ft<-cbind(coef(var_data)[[names[1]]][,1],
                coef(var_data)[[names[2]]][,1],
                coef(var_data)[[names[3]]][,1],
                coef(var_data)[[names[4]]][,1],
                coef(var_data)[[names[5]]][,1],
                coef(var_data)[[names[6]]][,1],
                coef(var_data)[[names[7]]][,1])
    }
  }
  
  
  
  
  IRF<-array(NA,dim=c(nsteps,nvar,ndraws))
  
  # Create the matrices for the loop
  #==================================
  y_artificial <- zeros(nobs+nlag,nvar)
  
  
  #Loop over the number of draws
  #==========================================================================
  
  tt <- 1; # numbers of accepted draws
  ww <- 1; # index for printing on screen
  
  while (tt<=ndraws){
    
    # Display number of loops
    if (tt==100*ww){
      print(tt)
      ww=ww+1
    }
    
    # STEP 1: choose the method and generate the residuals
    # Wild bootstrap based on simple distribution (~Rademacher)
    rr = 1-2*(rand(nobs,1)>0.5)
    u = resid*(rr%*%ones(1,nvar))
    Z = c(IV[1:nlag], IV[(nlag+1):length(IV)[1]]*rr)
    
    ## STEP 2: generate the artificial data
    ## STEP 2.1: initial values for the artificial data
    # Intialize the first nlag observations with real data
    LAG=NULL
    
    y_artificial<-matrix(NA,nobs+nlag,nvar)
    for (jj in 1:nlag){
      y_artificial[jj,] = ENDO[jj,]
      LAG = c(y_artificial[jj,], LAG) 
    }
    
    # Initialize the artificial series and the LAGplus vector
    T = t(1:nobs)
    if (const==0) {
      LAGplus = LAG
    } else if (const == 1) {
      LAGplus = c(1,LAG)
    } else if (const == 2) {
      print("dont supported")
    } else if (const == 3) {
      print("dont supported")
    }
    
    # STEP 2.2: generate artificial series
    # From observation nlag+1 to nobs, compute the artificial data
    for (jj in (nlag + 1):(nobs + nlag)) {
      for (mm in 1:nvar) {
        # Compute the value for time=jj
        y_artificial[jj, mm] = LAGplus %*% Ft[, mm] + u[jj - nlag, mm]
      }
      # now update the LAG matrix
      if (jj < (nobs + nlag)) {
        LAG = c(y_artificial[jj,], LAG[1:((nlag - 1) * nvar)])
        if (const == 0) {
          LAGplus = LAG
        } else if (const == 1) {
          LAGplus = c(1, LAG)
        } else if (const == 2) {
          print("dont supported")
        } else if (const == 3) {
          print("dont supported")
        }
      }
    }
    
    # STEP 3: estimate VAR on artificial data. 
    #STEP 4: calculate "ndraws" impulse responses and store them
    
    irf_draw<- suppressWarnings(var_inst(y_artificial,Z))  
    
    if (max(vars::roots(irf_draw$basevar))<.9999){
      IRF[,,tt] = irf_draw$IRF
      
      tt=tt+1
    }
  }
  print('-- Done!')
  
  # Compute the error bands
  #=========================
  pctg_inf <- (1-pctg)/2; 
  pctg_sup <- 1 - (1-pctg)/2;
  lower <- apply(IRF, c(1, 2), quantile, probs=pctg_inf)
  upper <- apply(IRF, c(1, 2), quantile, probs=pctg_sup)
  MED <- apply(IRF, c(1, 2), quantile, probs=0.50)
  BAR <- apply(IRF, c(1, 2), mean, probs=pctg_sup)
  colnames(lower)<-names
  colnames(upper)<-names
  colnames(MED)<-names
  colnames(BAR)<-names
  
  irf1<-list(irf=BAR,Lower=lower,Upper=upper,Median=MED,point=point)
  return(irf1)
}

var_type<-c("const") # choose from type = c("const", "trend", "both", "none")
irf_conf<-0.9 #Which Confidentintervals
irf_steps<-36 #Length of MA Representations
irf_runs<- 1000 # Number of Bootstrap samples
last_date<-as.POSIXlt("2019-06-01", tz = "UTC")
var_variable<-c("industry_sl","HICP_sl","PALLFNF_sl","Stoxx50_sl","ciss","DE5Y")
var_lag_order<-3 #How many lags to use
data_c <- subset(data,date>"2008-01-01")
qe_start<-as.POSIXlt("2014-10-01", tz = "UTC")
data_qe <- subset(data,date>=qe_start)
h <- 6.8 #7.02
w <- 11.8
units <- 'in'
dpi <- 1000
Endo <- NA
IV <- NA

##-------find propper instruments----
input<-c("OIS2Y",
         "DE2Y","DE5Y","DE10Y",
         "Euribor1m","Euribor3m","Euribor6m","Euribor1y",
         "govbond2y","govbond3y","govbond5y","govbond7y","govbond10y")
external.test<-data.frame(matrix(NA,length(shocks),length(input)))
colnames(external.test)<-input
rownames(external.test)<-shocks
for (j in 1:length(input)){print(input[j])
  for (i in 1:length(shocks)){
    if(shocks[i]!="QE"){
      get_data(data,c(var_variable[-length(var_variable)],input[j]),c(shocks[i]),border=last_date)
    } else {
      get_data(data_qe,c(var_variable[-length(var_variable)],input[j]),c(shocks[i]),border=last_date)
    }
    d<-var_inst(Endo,IV)
    external.test[i,j]<-d$f.robust
  }
}

external.test_melt<-cbind(MP=row.names(external.test),external.test)
external.test_melt<-melt(external.test_melt)
colnames(external.test_melt)[3]<-"Fstatistic"
external.test_melt$MP<-factor(external.test_melt$MP)
ggplot(external.test_melt)+
  geom_point(aes(variable,Fstatistic,color=MP),size=3,alpha=0.9)+
  xlab("")+
  theme_bw()+
  geom_hline(yintercept = 10)

#table paper
shocks<-c("Conventional","Timing","FG","QE","Unconventional","Total")
table_instr<-list()
table_r2<-rep(NA,length(shocks))
table_f<-rep(NA,length(shocks))
for (i in 1:length(shocks)){
  if(shocks[i]!="QE"){
    get_data(data,var_variable,c(shocks[i]),border=last_date) 
  }else {
    get_data(data_qe,var_variable,c(shocks[i]),border=last_date) 
  }
  d<-var_inst(Endo,IV)
  shocks.n<-c("Conventional","Timing","Forward Guidance","QE","Unconventional","Total")
  rownames(d$r.summary) <- c("Intercept",shocks.n[i])
  table_instr[[i]] <- d$r.summary
  table_r2[i] <- round(d$r2,3)
  table_f[i] <- round(d$f.robust,3)
}

writeLines(capture.output(stargazer(table_instr, dep.var.labels = c("residual DE5Y"),
                                    add.lines = list(c("R-squared",table_r2),
                                                     c("robust F-statistic",table_f)),
                                    title = "Regression of Z on Residuals",
                                    float = FALSE,
                                    single.row = TRUE)), "tables/fstat.tex")



##-------estimation----
##----------total----
get_data(data,var_variable,"Total",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
Total_irf<-irf_boot(d$basevar,d$IRF)
Total<-pretty.irf_complete(list(Total_irf),c("Total"))
ggsave("Graphics/Total.png",Total, height=h, width=w, units='in', dpi=dpi)
##----------conventional/unconventional----
get_data(data,var_variable,"Conventional",border=last_date)
d <- var_inst(Endo,IV);vars::roots(d$basevar)
Conventional_irf <- irf_boot(d$basevar,d$IRF)
Conventional <- pretty.irf_complete(list(Conventional_irf),c("Conventional"))
ggsave("Graphics/Conventional.png",Conventional, height=h, width=w, units='in', dpi=dpi)

get_data(data,var_variable,"Unconventional",border=last_date) 
d<-var_inst(Endo,IV);vars::roots(d$basevar)
Unconventional_irf<-irf_boot(d$basevar,d$IRF)
Unconventional<-pretty.irf_complete(list(Unconventional_irf),c("Unconventional"))
ggsave("Graphics/Unconventional.png",Unconventional, height=h, width=w, units='in', dpi=dpi)

#ConvUnconventional_comp<-pretty.irf_complete(list(Conventional_irf,Unconventional_irf),
#                                             c("Conventional","Unconventional"),leg=1)
ConvUnconventional_comp<-pretty.irf_sbs(list(Conventional_irf,Unconventional_irf),c("Conventional","Unconventional"),leg=1,same_scale = T)
ggsave("Graphics/ConventionalUnconventional_comp.png",ConvUnconventional_comp, height=h, width=w, units='in', dpi=dpi)

##----------Single factors----

get_data(data,var_variable,"Timing",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
Timing_irf<-irf_boot(d$basevar,d$IRF)
Timing<-pretty.irf_complete(list(Timing_irf),c("Timing"))
ggsave("Graphics/Timing.png",Timing, height=h, width=w, units='in', dpi=dpi)

get_data(data,var_variable,"FG",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
FG_irf<-irf_boot(d$basevar,d$IRF)
FG<-pretty.irf_complete(list(FG_irf),c("Forward Guidance"))
ggsave("Graphics/FG.png",FG, height=h, width=w, units='in', dpi=dpi)

get_data(data_qe,var_variable,"QE",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
QE_irf<-irf_boot(d$basevar,d$IRF)
QE<-pretty.irf_complete(list(QE_irf),c("QE"))
ggsave("Graphics/QE.png",QE, height=h, width=w, units='in', dpi=dpi)

#single_comp<-pretty.irf_complete(list(Conventional_irf,Timing_irf,FG_irf,QE_irf),
#                                 c("Conventional","Timing","Forward Guidance","QE"),leg=1)
single_comp<-pretty.irf_sbs(list(Conventional_irf,Timing_irf,FG_irf,QE_irf),c("Conventional","Timing","Forward Guidance","QE"),leg=1,same_scale = F)
ggsave("Graphics/single_comp.png",single_comp, height=h, width=w, units='in', dpi=dpi, limitsize = F)
##----------Delphic/Odyssean----

get_data(data,var_variable,"Conventional_odyss",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
a1<-irf_boot(d$basevar,d$IRF)
get_data(data,var_variable,"Conventional_delphic",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
a2<-irf_boot(d$basevar,d$IRF)
#Conventional_odydelp<-pretty.irf_complete(list(a1,a2),c("Odyssean","Delphic"),leg=1)
Conventional_odydelp<-pretty.irf_sbs(list(a1,a2),c("Odyssean","Delphic"),leg=1,same_scale = T)
ggsave("Graphics/Conventional_odydelp.png",Conventional_odydelp, height=h, width=w, units='in', dpi=dpi)

get_data(data,var_variable,"Timing_odyss",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
a1<-irf_boot(d$basevar,d$IRF)
get_data(data,var_variable,"Timing_delphic",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
a2<-irf_boot(d$basevar,d$IRF)
#Timing_odydelp<-pretty.irf_complete(list(a1,a2),c("Odyssean","Delphic"),leg=1)
Timing_odydelp<-pretty.irf_sbs(list(a1,a2),c("Odyssean","Delphic"),leg=1,same_scale = F)
ggsave("Graphics/Timing_odydelp.png",Timing_odydelp, height=h, width=w, units='in', dpi=dpi)

get_data(data,var_variable,"FG_odyss",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
a1<-irf_boot(d$basevar,d$IRF)
get_data(data,var_variable,"FG_delphic",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
a2<-irf_boot(d$basevar,d$IRF)
#FG_odydelp<-pretty.irf_complete(list(a1,a2),c("Odyssean","Delphic"),leg=1)
FG_odydelp<-pretty.irf_sbs(list(a1,a2),c("Odyssean","Delphic"),leg=1,same_scale = F)
ggsave("Graphics/FG_odydelp.png",FG_odydelp, height=h, width=w, units='in', dpi=dpi)

get_data(data,var_variable,"QE_odyss",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
a1<-irf_boot(d$basevar,d$IRF)
get_data(data,var_variable,"QE_delphic",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
a2<-irf_boot(d$basevar,d$IRF)
#QE_odydelp<-pretty.irf_complete(list(a1,a2),c("Odyssean","Delphic"),leg=1)
QE_odydelp<-pretty.irf_sbs(list(a1,a2),c("Odyssean","Delphic"),leg=1,same_scale = T)
ggsave("Graphics/QE_odydelp.png",QE_odydelp, height=h, width=w, units='in', dpi=dpi)

get_data(data,var_variable,"Total_odyss",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
a1<-irf_boot(d$basevar,d$IRF)
get_data(data,var_variable,"Total_delphic",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
a2<-irf_boot(d$basevar,d$IRF)
Total_odydelp<-pretty.irf_sbs(list(a1,a2),c("Odyssean","Delphic"),leg=1,same_scale = T)

##-------estimation Euribor-----
##----------conventional/unconventional----
get_data(data,c(var_variable[-length(var_variable)],"DE2Y"),"Conventional",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
Conventional_irf_DE2Y<-irf_boot(d$basevar,d$IRF)
Conventional<-pretty.irf_complete(list(Conventional_irf),c("Conventional"))

get_data(data,c(var_variable[-length(var_variable)],"DE2Y"),"Unconventional",border=last_date) 
d<-var_inst(Endo,IV);vars::roots(d$basevar)
Unconventional_irf_DE2Y<-irf_boot(d$basevar,d$IRF)
Unconventional<-pretty.irf_complete(list(Unconventional_irf),c("Unconventional"))

ConvUnconventional_comp<-pretty.irf_complete(list(Conventional_irf_DE2Y,Unconventional_irf_DE2Y),
                                             c("Conventional","Unconventional"),leg=1)
ggsave("Graphics/DE2Y/ConventionalUnconventionalEuribor.png",ConvUnconventional_comp, height=h, width=w, units='in', dpi=dpi)

##----------Single factors----
get_data(data,c(var_variable[-length(var_variable)],"DE2Y"),"Timing",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
Timing_irf_DE2Y<-irf_boot(d$basevar,d$IRF)
Timing<-pretty.irf_complete(list(Timing_irf),c("Timing"))
ggsave("Graphics/DE2Y/Timing.png",Timing, height=h, width=w, units='in', dpi=dpi)

get_data(data,c(var_variable[-length(var_variable)],"DE2Y"),"FG",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
FG_irf_DE2Y<-irf_boot(d$basevar,d$IRF)
FG<-pretty.irf_complete(list(FG_irf),c("Forward Guidance"))
ggsave("Graphics/DE2Y/FG.png",FG, height=h, width=w, units='in', dpi=dpi)

get_data(data_qe,c(var_variable[-length(var_variable)],"DE2Y"),"QE",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
QE_irf_DE2Y<-irf_boot(d$basevar,d$IRF)
QE<-pretty.irf_complete(list(QE_irf),c("QE"))
ggsave("Graphics/DE2Y/QE.png",QE, height=h, width=w, units='in', dpi=dpi)

single_comp<-pretty.irf_complete(list(Conventional_irf_DE2Y,Timing_irf_DE2Y,FG_irf_DE2Y,QE_irf_DE2Y),c("Conventional","Timing","Forward Guidance","QE"),leg=1)
ggsave("Graphics/DE2Y/single_comp.png",single_comp, height=h, width=w, units='in', dpi=dpi)

MP_comp<-pretty.irf_complete(list(Timing_irf_DE2Y,Timing_irf),c("DE2Y","DE5Y"),leg=1)
ggsave("Graphics/DE2Y/MP_comp.png",MP_comp, height=h, width=w, units='in', dpi=dpi)

##-------estimation qe period-----
##----------conventional/unconventional----

get_data(data_c,c(var_variable[-length(var_variable)],"DE5Y"),"Conventional",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
Conventional_irf<-irf_boot(d$basevar,d$IRF)
Conventional<-pretty.irf_complete(list(Conventional_irf),c("Conventional"),leg=1)

get_data(data_c,c(var_variable[-length(var_variable)],"DE5Y"),"Unconventional",border=last_date) 
d<-var_inst(Endo,IV);vars::roots(d$basevar)
Unconventional_irf<-irf_boot(d$basevar,d$IRF)
Unconventional<-pretty.irf_complete(list(Unconventional_irf),c("Unconventional"))

ConvUnconventional_comp<-pretty.irf_complete(list(Conventional_irf,Unconventional_irf),c("Conventional","Unconventional"),leg=1)
ggsave("Graphics/QE/ConventionalUnconventional_euribor_qe.png",ConvUnconventional_comp, height=h, width=w, units='in', dpi=dpi)

##----------Single factors----

get_data(data_c,c(var_variable[-length(var_variable)],"DE5Y"),"Timing",border=last_date,pri=T)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
Timing_irf<-irf_boot(d$basevar,d$IRF)
Timing<-pretty.irf_complete(list(Timing_irf),c("Timing"))
ggsave("Graphics/QE/Timing_irf.png",Timing, height=h, width=w, units='in', dpi=dpi)

get_data(data_c,c(var_variable[-length(var_variable)],"DE5Y"),"FG",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
FG_irf<-irf_boot(d$basevar,d$IRF)
FG<-pretty.irf_complete(list(FG_irf),c("Forward Guidance"))
ggsave("Graphics/QE/FG.png",FG, height=h, width=w, units='in', dpi=dpi)

get_data(data_qe,c(var_variable[-length(var_variable)],"DE5Y"),"QE",border=last_date)
d<-var_inst(Endo,IV);vars::roots(d$basevar)
QE_irf<-irf_boot(d$basevar,d$IRF)
QE<-pretty.irf_complete(list(QE_irf),c("QE"))
ggsave("Graphics/QE/QE.png",QE, height=h, width=w, units='in', dpi=dpi)

single_comp<-pretty.irf_complete(list(Conventional_irf,Timing_irf,FG_irf,QE_irf),c("Conventional","Timing","Forward Guidance","QE"),leg=1)
single_comp<-pretty.irf_complete(list(Conventional_irf,Timing_irf,QE_irf),c("Conventional","Timing","QE"),leg=1)
ggsave("Graphics/QE/single_comp.png",single_comp, height=h, width=w, units='in', dpi=dpi)


