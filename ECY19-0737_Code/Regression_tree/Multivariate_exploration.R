

require(foreach)
require(akima)
require(ggplot2)
load("multivariate_exploration/variance_community.Rdata")
setwd('multivariate_exploration/')


results=cbind(param_explor,foreach(i=1:length(variab),.combine=c)%do%{variab[[i]]})
colnames(results)=c('DMT','freq_H','rate_of_col','partial_loss','freq_Rand','Variance3D')

class=list()
class[[1]]<-results[,3]>=0.36&results[,5]>=0.44&results[,4]>=305/10000&results[,3]>=0.63&results[,5]>=0.69
class[[2]]<-results[,3]>=0.36&results[,5]>=0.44&results[,4]>=305/10000&results[,3]>=0.63&results[,5]<0.69
class[[3]]<-results[,3]>=0.36&results[,5]>=0.44&results[,4]>=305/10000&results[,3]<0.63
class[[4]]<-results[,3]>=0.36&results[,5]>=0.44&results[,4]<305/10000

class[[5]]<-results[,3]>=0.36&results[,5]<0.44&results[,1]>=83.25&results[,4]>=371/10000&results[,5]>=.14
class[[6]]<-results[,3]>=0.36&results[,5]<0.44&results[,1]>=83.25&results[,4]>=371/10000&results[,5]<.14
class[[7]]<-results[,3]>=0.36&results[,5]<0.44&results[,1]>=83.25&results[,4]<371/10000
class[[8]]<-results[,3]>=0.36&results[,5]<0.44&results[,1]<83.25&results[,5]>=.36
class[[9]]<-results[,3]>=0.36&results[,5]<0.44&results[,1]<83.25&results[,5]<.36

class[[10]]<-results[,3]<0.36&results[,1]>=117.7&results[,5]>=0.24&results[,3]>=0.1&results[,4]>=352/10000
class[[11]]<-results[,3]<0.36&results[,1]>=117.7&results[,5]>=0.24&results[,3]>=0.1&results[,4]<352/10000
class[[12]]<-results[,3]<0.36&results[,1]>=117.7&results[,5]>=0.24&results[,3]<0.1
class[[13]]<-results[,3]<0.36&results[,1]>=117.7&results[,5]<0.24

class[[14]]<-results[,3]<0.36&results[,1]<117.7&results[,5]>=0.31&results[,3]>=.12&results[,1]>=48.2
class[[15]]<-results[,3]<0.36&results[,1]<117.7&results[,5]>=0.31&results[,3]>=.12&results[,1]<48.2
class[[16]]<-results[,3]<0.36&results[,1]<117.7&results[,5]>=0.31&results[,3]<.12
class[[17]]<-results[,3]<0.36&results[,1]<117.7&results[,5]<0.31&results[,1]>=22.5
class[[18]]<-results[,3]<0.36&results[,1]<117.7&results[,5]<0.31&results[,1]<22.5



pdf("multivariate_exploration/variance_explor.pdf")


for(i in 1:length(class)){
  
  hist(results[which(class[[i]]),6],col='grey',xlim=c(0,max(results[,6])),n=70,main=i)
}
dev.off()

to_plot=foreach(i=1:length(class))%do%{
  results[which(class[[i]]),6]
}


##########################################################################################
##########################################################################################



load("multivariate_exploration/variance_community2.Rdata")


results=cbind(param_explor[1:100,],foreach(i=1:length(variab2),.combine=rbind)%do%{variab2[[i]]})
colnames(results)=c('DMT','freq_H','rate_of_col','partial_loss','freq_Rand','growth','recruit','Variance3D')


class=list()
class[[1]]<-results[,3]>=0.36&results[,5]>=0.44&results[,4]>=305/10000&results[,3]>=0.63&results[,5]>=0.69
class[[2]]<-results[,3]>=0.36&results[,5]>=0.44&results[,4]>=305/10000&results[,3]>=0.63&results[,5]<0.69
class[[3]]<-results[,3]>=0.36&results[,5]>=0.44&results[,4]>=305/10000&results[,3]<0.63
class[[4]]<-results[,3]>=0.36&results[,5]>=0.44&results[,4]<305/10000

class[[5]]<-results[,3]>=0.36&results[,5]<0.44&results[,1]>=83.25&results[,4]>=371/10000&results[,5]>=.14
class[[6]]<-results[,3]>=0.36&results[,5]<0.44&results[,1]>=83.25&results[,4]>=371/10000&results[,5]<.14
class[[7]]<-results[,3]>=0.36&results[,5]<0.44&results[,1]>=83.25&results[,4]<371/10000
class[[8]]<-results[,3]>=0.36&results[,5]<0.44&results[,1]<83.25&results[,5]>=.36
class[[9]]<-results[,3]>=0.36&results[,5]<0.44&results[,1]<83.25&results[,5]<.36

class[[10]]<-results[,3]<0.36&results[,1]>=117.7&results[,5]>=0.24&results[,3]>=0.1&results[,4]>=352/10000
class[[11]]<-results[,3]<0.36&results[,1]>=117.7&results[,5]>=0.24&results[,3]>=0.1&results[,4]<352/10000
class[[12]]<-results[,3]<0.36&results[,1]>=117.7&results[,5]>=0.24&results[,3]<0.1
class[[13]]<-results[,3]<0.36&results[,1]>=117.7&results[,5]<0.24

class[[14]]<-results[,3]<0.36&results[,1]<117.7&results[,5]>=0.31&results[,3]>=.12&results[,1]>=48.2
class[[15]]<-results[,3]<0.36&results[,1]<117.7&results[,5]>=0.31&results[,3]>=.12&results[,1]<48.2
class[[16]]<-results[,3]<0.36&results[,1]<117.7&results[,5]>=0.31&results[,3]<.12
class[[17]]<-results[,3]<0.36&results[,1]<117.7&results[,5]<0.31&results[,1]>=22.5
class[[18]]<-results[,3]<0.36&results[,1]<117.7&results[,5]<0.31&results[,1]<22.5





pdf("multivariate_exploration/variance_explor2.pdf")

for(i in 1:length(class)){
  boxplot(cbind(results[which(class[[i]]),6],results[which(class[[i]]),7])/(results[which(class[[i]]),6]+results[which(class[[i]]),7]),col='grey',main=i,las=1)
}
dev.off()


##############################################################################################################
##############################################################################################################

load("multivariate_exploration/variance_community3.Rdata")


results=cbind(param_explor[1:5000,],foreach(i=1:length(variab2),.combine=rbind)%do%{variab2[[i]]})
colnames(results)=c('DMT','freq_H','rate_of_col','partial_loss','freq_Rand','Variance3D','growth','recruit')


class=list()
class[[1]]<-results[,3]>=0.36&results[,5]>=0.44&results[,4]>=305/10000&results[,3]>=0.63&results[,5]>=0.69
class[[2]]<-results[,3]>=0.36&results[,5]>=0.44&results[,4]>=305/10000&results[,3]>=0.63&results[,5]<0.69
class[[3]]<-results[,3]>=0.36&results[,5]>=0.44&results[,4]>=305/10000&results[,3]<0.63
class[[4]]<-results[,3]>=0.36&results[,5]>=0.44&results[,4]<305/10000

class[[5]]<-results[,3]>=0.36&results[,5]<0.44&results[,1]>=83.25&results[,4]>=371/10000&results[,5]>=.14
class[[6]]<-results[,3]>=0.36&results[,5]<0.44&results[,1]>=83.25&results[,4]>=371/10000&results[,5]<.14
class[[7]]<-results[,3]>=0.36&results[,5]<0.44&results[,1]>=83.25&results[,4]<371/10000
class[[8]]<-results[,3]>=0.36&results[,5]<0.44&results[,1]<83.25&results[,5]>=.36
class[[9]]<-results[,3]>=0.36&results[,5]<0.44&results[,1]<83.25&results[,5]<.36

class[[10]]<-results[,3]<0.36&results[,1]>=117.7&results[,5]>=0.24&results[,3]>=0.1&results[,4]>=352/10000
class[[11]]<-results[,3]<0.36&results[,1]>=117.7&results[,5]>=0.24&results[,3]>=0.1&results[,4]<352/10000
class[[12]]<-results[,3]<0.36&results[,1]>=117.7&results[,5]>=0.24&results[,3]<0.1
class[[13]]<-results[,3]<0.36&results[,1]>=117.7&results[,5]<0.24

class[[14]]<-results[,3]<0.36&results[,1]<117.7&results[,5]>=0.31&results[,3]>=.12&results[,1]>=48.2
class[[15]]<-results[,3]<0.36&results[,1]<117.7&results[,5]>=0.31&results[,3]>=.12&results[,1]<48.2
class[[16]]<-results[,3]<0.36&results[,1]<117.7&results[,5]>=0.31&results[,3]<.12
class[[17]]<-results[,3]<0.36&results[,1]<117.7&results[,5]<0.31&results[,1]>=22.5
class[[18]]<-results[,3]<0.36&results[,1]<117.7&results[,5]<0.31&results[,1]<22.5

pdf("multivariate_exploration/variance_explor2.pdf")

for(i in 1:length(class)){
  boxplot(cbind(results[which(class[[i]]),6],results[which(class[[i]]),7])/(results[which(class[[i]]),6]+results[which(class[[i]]),7]),col='grey',main=i,las=1)
}
dev.off()


pdf("/multivariate_exploration/pie_growth_recruit.pdf",height=5, width=30)

par(mfrow=c(1,18))
for(i in 1:length(class)){
  pie(apply(cbind(results[which(class[[i]]),6],results[which(class[[i]]),7])/(results[which(class[[i]]),6]+results[which(class[[i]]),7]),2,mean),labels='',main=i)
}
dev.off()

plot3d(results[,5],results[,3],results[,6]/(results[,6]+results[,7]),col='dodgerblue')


############################################### Partial mortality
####### growth
pdf('growthns.pdf',width=10,height=10)
results=as.data.frame(results)
fld <- with(results, interp(x = rate_of_col, 
                       y = freq_Rand, 
                       z = growth,
                       xo = seq(min(rate_of_col), max(rate_of_col), length=100),
                       yo = seq(min(freq_Rand), max(freq_Rand), length=100),
                       duplicate="mean"))

gdat <- interp2xyz(fld, data.frame=TRUE)

require(ggplot2)
ggplot(gdat) + 
  aes(x = x, y = y, z = z, fill = z) + 
  geom_tile() + 
  coord_equal() +
  labs(y='frequence of partial mortality events',x='proportion of colonies touched by partial mortality events')+
  geom_contour(color = "white", alpha = 0.5) + 
  scale_fill_distiller(palette="Spectral", na.value="white") + 
  theme_bw()
dev.off()


####### recruit
pdf('recruitns.pdf',width=10,height=10)
results=as.data.frame(results)
fld <- with(results, interp(x = rate_of_col, 
                            y = freq_Rand, 
                            z = recruit,#/(growth+recruit),
                            xo = seq(min(rate_of_col), max(rate_of_col), length=100),
                            yo = seq(min(freq_Rand), max(freq_Rand), length=100),
                            duplicate="mean"))

gdat <- interp2xyz(fld, data.frame=TRUE)

require(ggplot2)
ggplot(gdat) + 
  aes(x = x, y = y, z = z, fill = z) + 
  geom_tile() + 
  coord_equal() +
  labs(y='frequence of partial mortality events',x='proportion of colonies touched by partial mortality events')+
  geom_contour(color = "white", alpha = 0.5) + 
  scale_fill_distiller(palette="Spectral", na.value="white") + 
  theme_bw()
dev.off()


####### recruit VS growth
pdf('recruitvsgrowth2.pdf',width=10,height=10)
results=as.data.frame(results)
fld <- with(results, interp(x = rate_of_col, 
                            y = freq_Rand, 
                            z = recruit/(growth+recruit),
                            xo = seq(min(rate_of_col), max(rate_of_col), length=100),
                            yo = seq(min(freq_Rand), max(freq_Rand), length=100),
                            duplicate="mean"))

gdat <- interp2xyz(fld, data.frame=TRUE)


ggplot(gdat) + 
  aes(x = x, y = y, z = z, fill = z) + 
  geom_tile() + 
  coord_equal() +
  labs(y='frequence of partial mortality events',x='proportion of colonies touched by partial mortality events')+
  geom_contour(bins=5,color = "white", alpha = .5) + 
  scale_fill_distiller(palette="RdBu", na.value="white") + 
  theme_bw()+
theme(axis.text=element_text(size=16),axis.title=element_text(size=18,face="bold"))
dev.off()



####### recruit + growth
pdf('recruit+growth2.pdf',width=10,height=10)
results=as.data.frame(results)
fld <- with(results, interp(x = rate_of_col, 
                            y = freq_Rand, 
                            z = growth+recruit,
                            xo = seq(min(rate_of_col), max(rate_of_col), length=100),
                            yo = seq(min(freq_Rand), max(freq_Rand), length=100),
                            duplicate="mean"))

gdat <- interp2xyz(fld, data.frame=TRUE)

require(ggplot2)
ggplot(gdat) + 
  aes(x = x, y = y, z = z, fill = z) + 
  geom_tile() + 
  coord_equal() +
  labs(y='frequence of partial mortality events',x='proportion of colonies touched by partial mortality events')+
  geom_contour(bins=5,color = "white", alpha = .5) + 
  scale_fill_distiller(palette="RdBu", na.value="white") + 
  theme_bw()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18,face="bold"))
dev.off()


####### variability


### PANEL A
require(ggplot2)
pdf('variability2v2_test.pdf',width=10,height=10)
results=as.data.frame(results)
fld <- with(results, interp(x = rate_of_col, 
                            y = freq_Rand, 
                            z = Variance3D,
                            xo = seq(min(rate_of_col), max(rate_of_col), length=100),
                            yo = seq(min(freq_Rand), max(freq_Rand), length=100),
                            duplicate="mean"))

gdat <- interp2xyz(fld, data.frame=TRUE)

gdat[which(gdat[,3]>=25),3]=25

ggplot(gdat) + 
  aes(x = x, y = y, z = z, fill = z) + 
  geom_tile() + 
  labs(y='probability of partial mortality events',x='proportion of colonies touched by partial mortality events')+
  geom_contour(bins=3,color = "white", alpha = .5) + 
  scale_fill_distiller(palette="RdBu", na.value="white") + 
  theme_bw()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18,face="bold"))
dev.off()


### PANEL B
require(ggplot2)
pdf('variability2v2_HYDRO.pdf',width=10,height=10)
#results=as.data.frame(results)
fld <- with(results, interp(x = DMT, 
                            y = freq_H, 
                            z = Variance3D,
                            xo = seq(min(DMT), max(DMT), length=100),
                            yo = seq(min(freq_H), max(freq_H), length=100),
                            duplicate="mean"))

gdat <- interp2xyz(fld, data.frame=TRUE)

gdat[which(gdat[,3]>=25),3]=25

ggplot(gdat) + 
  aes(x = x, y = y, z = z, fill = z) + 
  geom_tile() + 
  labs(y='probability of whole mortality events',x='Intensity of an hydrodynamic event')+
  geom_contour(bins=3,color = "white", alpha = .5) + 
  scale_fill_distiller(palette="RdBu", na.value="white") + 
  theme_bw()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18,face="bold"))
dev.off()






pdf('extractD.pdf',width=5,height=5)
x=gdat[which(gdat[,2]==gdat[4500,2]),1][2:99]
y=gdat[which(gdat[,2]==gdat[4500,2]),3][2:99]
plot(x,y,pch=16,col='grey',las=1,ylim=c(0,30))
bb=loess(y~x,span=1)
lines(x,bb$fitted,lwd=3,lty=3)
dev.off()

pdf('extractC.pdf',width=5,height=5)
x=gdat[which(gdat[,1]==gdat[9985,1]),2][2:99]
  y=gdat[which(gdat[,1]==gdat[9985,1]),3][2:99]
plot(x,y,pch=16,col='black',las=1,ylim=c(0,30))
bb=loess(y~x,span=2)
#lines(x,bb$fitted,lwd=3,col='grey')
dev.off()

pdf('extractB.pdf',width=5,height=5)
x=gdat[which(gdat[,1]==gdat[46,1]),2][2:99]
y=gdat[which(gdat[,1]==gdat[46,1]),3][2:99]
plot(x,y,pch=16,col='black',las=1,ylim=c(0,30))
bb=loess(y~x,span=1)
lines(x,bb$fitted,lwd=3,lty=3,col='grey')
dev.off()

############################################### hydro mortality
####### growth
pdf('growthH.pdf',width=10,height=10)
results=as.data.frame(results)
fld <- with(results, interp(x = DMT, 
                            y = freq_H, 
                            z = growth/(growth+recruit),
                            xo = seq(min(DMT), max(DMT), length=100),
                            yo = seq(min(freq_H), max(freq_H), length=100),
                            duplicate="mean"))

gdat <- interp2xyz(fld, data.frame=TRUE)

require(ggplot2)
ggplot(gdat) + 
  aes(x = x, y = y, z = z, fill = z) + 
  geom_tile() +
  labs(y='frequence of whole mortality events',x='intensity of whole mortality events')+
  geom_contour(color = "white", alpha = 0.5) + 
  scale_fill_distiller(palette="Spectral", na.value="white") + 
  theme_bw()
dev.off()


####### recruit
pdf('recruitH.pdf',width=10,height=10)
results=as.data.frame(results)
fld <- with(results, interp(x = DMT, 
                            y = freq_H, 
                            z = recruit/(growth+recruit),
                            xo = seq(min(DMT), max(DMT), length=100),
                            yo = seq(min(freq_H), max(freq_H), length=100),
                            duplicate="mean"))

gdat <- interp2xyz(fld, data.frame=TRUE)

require(ggplot2)
ggplot(gdat) + 
  aes(x = x, y = y, z = z, fill = z) + 
  geom_tile() +
  labs(y='frequence of whole mortality events',x='intensity of whole mortality events')+
  geom_contour(color = "white", alpha = 0.5) + 
  scale_fill_distiller(palette="Spectral", na.value="white") + 
  theme_bw()
dev.off()




