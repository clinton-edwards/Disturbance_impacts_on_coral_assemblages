

#require(doMC)

model_full<-function(tmax,DMTc,DMTe,frequency,plasticity,mu,part,frequency_death){
  
  ####################################################
  ####################################################
  madin_data=read.csv('Madin_data_TRUE.csv',header=T)  
  prop_3d_F=matrix(NA,3,20)
  
  plasticity=plasticity
  
  model=list()
  
  for(i in 1:length(unique(madin_data$Group))){
    #plot(madin_data[which(madin_data$Group==unique(madin_data$Group)[i]),2],madin_data[which(madin_data$Group==unique(madin_data$Group)[i]),3],las=1,main=unique(madin_data$Group)[i],ylab='ln(CSF)',xlab='ln(Size)',ylim=c(-3,7))
    model[[i]]=lm(madin_data[which(madin_data$Group==unique(madin_data$Group)[i]),3]~madin_data[which(madin_data$Group==unique(madin_data$Group)[i]),2])
    #abline(model[[i]],col='red',lwd=2)
    
  }
  names(model)=unique(madin_data$Group)
  
  bob=t(as.data.frame(lapply(model,coefficients)))
  coef_model_CSF=t(bob)
  rownames(coef_model_CSF)=c('intercept','slope')
  ## modeling the link between  
  summary(lm(bob[-1,2]~bob[-1,1]))
  
  sd_tabular=sd(model$tabular$residuals)
  sd_digitate=sd(model$digitate$residuals)
  sd_massive=sd(model$massive$residuals)
  coef_model_CSF=coef_model_CSF[,c(5,3,4)]
  
  res_tabular=model$tabular$residuals
  res_digitate=model$digitate$residuals
  res_massive=model$massive$residuals
  res_model=list(res_tabular,res_digitate,res_massive)
  
  sd_res=c(sd_tabular,sd_digitate,sd_massive)
  names(sd_res)=colnames(coef_model_CSF)
  ####################################################
  ####################################################
  
  
  
  ####################################################
  ####################################################
  
  CSF=function(x,type,res_model,plast){
    if(x>0){
      #ln_CSF=coef_model_CSF[2,type]*log(x)+coef_model_CSF[1,type]+plast*rnorm(1,mean=0,sd=sd_res[type])
      ln_CSF=coef_model_CSF[2,type]*log(x)+coef_model_CSF[1,type]+plast*as.numeric(sample(res_model[[type]],1,replace=T))
      
      return(exp(ln_CSF))
    }else{ln_CSF=0
          return(0)}
    
  }
  
  
  growth=function(x,g,C,Area){
    if(x>0){
      res=x+(2*pi*g*sqrt(x/pi)+pi*g^2)*max(((Area-sum(C))/Area),0)
    }else{res=x}
    return(res)
  }
  
  death=function(C,mu,part,rec_size){
    
    if(length(which(C>0))>0){
      who=which(runif(length(which(C>0)))<mu)
      C[which(C>0)[who]]=C[which(C>0)[who]]-part # partial
      C[which(C<rec_size)]=0
      dead=length(who)
    }else{C=C
          dead=0
    }
    return(list(C,dead))
  }
  
  death_HYDRO=function(C,type,CSF_value,DMT){
    if(length(which(C>0))>0){
      C[which(CSF_value>DMT)]=0
      dead=length(which(CSF_value>DMT))
    }else{C=C
          dead=0}
    return(list(C,dead))
  }
  
  
  
  
  
  recruit=function(C,rec_rate,rec_size,Area,coltype){
    if(length(which(C==0))>0){
      where=sample(which(C==0),min(floor(rec_rate*(Area-sum(C))),length(which(C==0))))
      C[where]=rec_size
      coltype[where]=sample(3,length(where),replace=T)
    }else{
      where=0
      C=C
      coltype=coltype
    }
    
    return(list(C,coltype,where))
  }
  ####################################################
  ####################################################
  
  
  ####################################################
  ####################################################
  tmax=tmax
  n=50*50
  Area=25
  mu=mu
  rec_rate=25 # from Sandin McNamara
  rec_size=Area/n
  plast=plasticity
  frequency=frequency
  
  #g=rep(0.15,n)
  
  
  colonies=matrix(0,n,tmax)
  type=matrix(NA,n,tmax)
  rec_cover=matrix(0,1,tmax)
  pure_growth=matrix(0,1,tmax)
  CSF_value=matrix(0,n,tmax)
  num_col=rep(0,tmax)
  #colonies[,1]=rep(Area/(3*n),n)
  
  
  dead=vector()

  DMTe=DMTe
  DMTc=DMTc
  
  for(t in 2:tmax){
    #cat(t,fill=T)
    
    
    if(runif(1)<=frequency_death){la_mort=1
    }else{la_mort=0}
    
    if(runif(1)<=frequency){DMT=DMTe
    }else{DMT=DMTc}
    
    g=rep(0,n)
    
    colonies[,t]=colonies[,t-1]
    type[,t]=type[,t-1]
    CSF_value[,t]=CSF_value[,t-1]
    # RECRUIT ####################################################
    
    recruitment=recruit(colonies[,t],rec_rate,rec_size,Area,type[,t])
    colonies[,t]=recruitment[[1]]
    type[,t]=recruitment[[2]]
    rec_cover[,t]=length(recruitment[[3]])*rec_size
    if(sum(colonies[,t])>25){stop('error #1')}
    g[which(type[,t]==3)]=0.007
    g[which(type[,t]==2)]=0.022
    g[which(type[,t]==1)]=0.055
    # GROWTH ####################################################
    mix=sample(n)
    for(i in 1:n){
      colonies[mix[i],t]=growth(colonies[mix[i],t],g[mix[i]],colonies[,t],Area) 
      CSF_value[mix[i],t]=CSF(colonies[mix[i],t],type[mix[i],t],res_model,plast)
    }
    if(t>1){
    pure_growth[t]=sum(colonies[,t]-colonies[,t-1])
    }
    if(sum(colonies[,t])>25){stop('error #2')}
    # DEATH Natural ####################################################
    if(la_mort==1){
    muerte=death(colonies[,t],mu,part,rec_size)
    colonies[,t]=muerte[[1]]
    dead_rnd=muerte[[2]]
    }
    
    muerte=death_HYDRO(colonies[,t],type[,t],CSF_value[,t],DMT)
    colonies[,t]=muerte[[1]]
    if(la_mort==1){
    dead[t]=muerte[[2]]+dead_rnd}else{dead[t]=muerte[[2]]}
    if(sum(colonies[,t])>25){stop('error #3')}
    # DEATH Hydrodynamic ####################################################
    #colonies[,t]=death_HYDRO(colonies[,t],type)
    
    
    num_col[t]=length(which(colonies[,t]>0))
    
  }
  
  
  
  prop3d=foreach(j=1:tmax,.combine=cbind)%do%{
    # unlist(aggregate(colonies[,j],list(type[,j]),sum)[,2])
    foreach(jj=1:3,.combine=c)%do%{
      sum(colonies[which(type[,j]==jj),j])}
  }
  
  
  
  
  resultat=list(colonies,type,CSF_value,prop3d,dead,rec_cover,pure_growth)
  names(resultat)=c('colonies','type','CSF','proportion','deaths','recruit_cover','pure_growth')
  return(resultat)
}






