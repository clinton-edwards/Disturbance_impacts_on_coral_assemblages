setwd('multivariate_exploration')
dir()
load('Model_full_CSF_Full_EXPLO_10000.Rdata')
resultats1=resultats
load('Model_full_CSF_Full_EXPLO_10000_2.Rdata')
resultats
super_result=as.data.frame(resultats)
super_result1=as.data.frame(resultats1)
super_result=rbind(super_result,super_result1)


error.bars=function(y,z){
  x=barplot(y,plot=F)
  n=length(y)
  for(i in 1:n){
    arrows(x[i],y[i]-z,x[i],y[i]+z,code=3,angle=90,length=0,lwd=2)}
}

require(rgl)
plot3d(super_result$Tabular_cover,super_result$Digitate_cover,super_result$Massive_cover,size = .5,xlab='Tabular cover',ylab='Digitate cover',zlab='Massive cover')

par(mfrow=c(1,3),mar=c(5,5,5,5))
plot(super_result$Tabular_cover,super_result$Tabular_mean_size,pch=16,cex=.1)
plot(super_result$Digitate_cover,super_result$Digitate_mean_size,pch=16,cex=.1)
plot(super_result$Massive_cover,super_result$Massive_mean_size,pch=16,cex=.1)


param=as.matrix(super_result[,1:5])
param1=as.matrix(super_result1[,1:5])




par(mfrow=c(1,1))
par(mar=c(3,1,3,1))
arbre=rpart(apply(super_result[,6:8],1,sum)~param)
plot(arbre)
text(arbre,cex=.7)

source(file = 'mymet.r')
response_var=as.matrix(super_result[,6:8])
response_var1=as.matrix(super_result1[,6:8])
form_tree=as.formula(response_var~param)
form_tree1=as.formula(response_var1~param1)
arbre=rpart(form_tree,method = mymet)
arbre1=rpart(form_tree1,method = mymet)


par(mfrow=c(1,1))
arbrep=prune(arbre,cp=0.01)
plot(arbrep,uniform = T,branch = 1)
text(arbrep,split=T,cex=.5)

means=arbrep$frame[which(arbrep$frame[,1]=='<leaf>'),]

pdf('tree_20000_0015_g2new.pdf',height=6,width=36)
par(mfrow=c(1,nrow(means)))
for(i in 1:nrow(means)){
  trace=c(unlist(means[i,])[9:11],sum(unlist(means[i,])[9:11]))
  names(trace)=c('Tabular','Digitate','Massive','Global')
  barplot(trace,col=c('red','green','blue','grey'),las=2,ylim=c(0,1),space=0)
}
dev.off()

pdf('tree_20000_001new.pdf',height=5,width=36)
par(mfrow=c(1,1+nrow(means)))
for(i in 1:nrow(means)){
  trace=c(unlist(means[i,])[9:11])
  names(trace)=c('Tabular','Digitate','Massive')
  barplot(trace,col=c('red','green','blue'),las=2,ylim=c(0,1),space=0)
}
dev.off()

pdf('tree_20000_001pie.pdf',height=5,width=36)
par(mfrow=c(1,nrow(means)))
for(i in 1:nrow(means)){
  pie(unlist(means[i,])[9:11],col=c('red','green','blue'),radius=1.1*sum(unlist(means[i,])[9:11]),labels='',border = 0)
}
dev.off()
pdf('tree_20000_001pieBK.pdf',height=5,width=36)
par(mfrow=c(1,nrow(means)))
for(i in 1:nrow(means)){
  pie(1,col='grey',radius=1,labels='',border = 0)
  }
dev.off()

pie(unlist(means[i,])[9:11],col=c('red','green','blue'),radius=1.1*sum(unlist(means[i,])[9:11]),labels='')
pie(col='grey',radius=1,labels='')





pdf('tree_fig_4_0015_raw.pdf',width=10,height=5)
plot(arbrep,uniform = T,branch = 1)
text(arbrep,split=T,cex=.5)
plot(arbrep,uniform = T,branch = 1,lwd=3)
dev.off()

require(rgl)
plot3d(super_result$Tabular_cover,super_result$Digitate_cover,super_result$Massive_cover,size =1,col=arbrep$where)






sub_par=param[sample(20000,20000,replace=F),]
sub_var=response_var[sample(20000,20000,replace=F),]
form_tree=as.formula(sub_var~sub_par)
arbre2=rpart(form_tree,method = mymet)
plot(arbre2)
text(arbre2,cex=.7)

par(mar=c(1,1,1,1))
plot(arbre)
text(arbre,cex=.7)
prp(arbre, faclen = 0, cex = 0.8, extra = 1)
sub_par=param[sample(10000,5000),]
  sub_var=response_var[sample(10000,5000),]
form_tree=as.formula(sub_var~sub_par)
arbre=rpart(form_tree,method = mymet)
plot(arbre)
text(arbre,cex=.7)

response_cover=response_var[,1:3]
form_tree=as.formula(response_cover~param)
arbre_p=rpart(form_tree,method = mymet)

form_tree_c=as.formula(response_var[,3]~param)
arbre_c=rpart(form_tree_c)

form_tree_m=as.formula(response_var[,4:6]~param)
arbre_m=rpart(form_tree_c,method = mymet)

form_tree=as.formula(apply(response_cover,1,sum)~param)
arbre_cover=rpart(form_tree)

form_tree=as.formula(apply(response_cover,1,sum)~param)
arbre_cover=rpart(form_tree)




arbre=prune(arbre_p,cp=0.02)
arbre=arbre_cover
#par(mfrow=c(1,1))
#par(mar=c(1,1,1,1))
#plot(arbre)
#text(arbre,cex=.7,pos=1)

nodes_content=split(super_result,arbre$where)

pdf('class_from_tree_new_range_cover_005.pdf',height=8,width=13)
plot(arbre)
text(arbre)


par(mfrow=c(1,5))
par(mar=c(12,3,3,3))
boxplot(nodes_content[[i]][,1],las=2,ylim=c(0,max(super_result$DMT)))
boxplot(nodes_content[[i]][,c(2,5)],las=2,ylim=c(0,max(super_result$freq_H)))
boxplot(nodes_content[[i]][,c(3,4)],las=2,ylim=c(0,1))
boxplot(nodes_content[[i]][,6:8],las=2,ylim=c(0,1),col=c('red','green','blue'))
boxplot(nodes_content[[i]][,9:11],las=2,col=c('red','green','blue'))
}
dev.off()





  pdf(paste(i,'__FULLcover.pdf'),height=6,width=40)
  par(mfrow=c(1,13))
  par(mar=c(19,5,3,3))
for(i in 1:length(nodes_content)){
  #boxplot(nodes_content[[i]][,1],las=2,ylim=c(0,max(super_result$DMT)))
  #boxplot(nodes_content[[i]][,c(2,5)],las=2,ylim=c(0,max(super_result$freq_H)))
  #boxplot(nodes_content[[i]][,c(3,4)],las=2,ylim=c(0,1))
  boxplot(nodes_content[[i]][,6:8],las=2,col=c('red','green','blue'),ylim=c(0,1),cex.axis=3)
  #boxplot(nodes_content[[i]][,9:11],las=2,col=c('red','green','blue'))

}
dev.off()


pdf('tree.pdf',height=8,width=13)
plot(arbre)
dev.off()


########################################################################################

nodes_content_simple=split(super_result,arbre_simpler$where)

pdf(paste(i,'__FULLcover_simpler_tree.pdf'),height=6,width=40)
par(mfrow=c(1,11))
par(mar=c(19,5,3,3))
for(i in 1:length(nodes_content_simple)){
  #boxplot(nodes_content[[i]][,1],las=2,ylim=c(0,max(super_result$DMT)))
  #boxplot(nodes_content[[i]][,c(2,5)],las=2,ylim=c(0,max(super_result$freq_H)))
  #boxplot(nodes_content[[i]][,c(3,4)],las=2,ylim=c(0,1))
  boxplot(nodes_content_simple[[i]][,6:8],las=2,col=c('red','green','blue'),ylim=c(0,0.9),cex.axis=3)
  #boxplot(nodes_content[[i]][,9:11],las=2,col=c('red','green','blue'))
  
}
dev.off()


pdf('tree_simple.pdf',height=8,width=13)
plot(arbre_simpler)
plot(arbre_simpler)
text(arbre_simpler)
dev.off()



############################# conditional tree
test=super_result
colnames(test)
pdf('cttree.pdf',height=20,width=20)
plot(ctree(Tabular_cover+Digitate_cover+Massive_cover~DMT+freq_H+rate_of_col+partial_loss+freq_Rand,data=test))
dev.off()


