setwd('multivariate_exploration')
dir()
load('Model_full_CSF_RAND_EXPLO_10X30X30.Rdata')
super_result=as.data.frame(resultats)
resol=30
pdf('mortality_rnd_impact_0_24.pdf',height=5,width=10)
for(jj in 1:30){
  mu=unique(Low_random$rate_of_col)[jj]
  par(mfrow=c(1,3))
  
  Low_random=super_result[which(super_result$partial_loss==unique(super_result$partial_loss)[1]),]
  plot(Low_random$freq_Rand[which(Low_random$rate_of_col==mu)],Low_random$Tabular_cover[which(Low_random$rate_of_col==mu)],main='Tabular',ylim=c(0,1),las=1,lwd=3,col=rgb(0,0,0,alpha=1),type='l',xlab='event probability',ylab='cover')
  Low_random=super_result[which(super_result$partial_loss==unique(super_result$partial_loss)[2]),]
  lines(Low_random$freq_Rand[which(Low_random$rate_of_col==mu)],Low_random$Tabular_cover[which(Low_random$rate_of_col==mu)],las=1,type='l',lwd=3,col=rgb(0,0,0,alpha=.75))
  Low_random=super_result[which(super_result$partial_loss==unique(super_result$partial_loss)[3]),]
  lines(Low_random$freq_Rand[which(Low_random$rate_of_col==mu)],Low_random$Tabular_cover[which(Low_random$rate_of_col==mu)],las=1,type='l',lwd=3,col=rgb(0,0,0,alpha=.5))
  Low_random=super_result[which(super_result$partial_loss==unique(super_result$partial_loss)[9]),]
  lines(Low_random$freq_Rand[which(Low_random$rate_of_col==mu)],Low_random$Tabular_cover[which(Low_random$rate_of_col==mu)],las=1,type='l',lwd=3,col=rgb(0,0,0,alpha=.1))
  legend(x=0,y=1,c('Ko = 100','Ko = 600','Ko = 1200','Ko = 4500'),bty='n',lty=c(1,1,1,1),lwd=c(2,2,2,2),col=c(rgb(0,0,0,alpha=1),rgb(0,0,0,alpha=.75),rgb(0,0,0,alpha=.25),rgb(0,0,0,alpha=.1)),cex=1) 
  
  Low_random=super_result[which(super_result$partial_loss==unique(super_result$partial_loss)[1]),]
  plot(Low_random$freq_Rand[which(Low_random$rate_of_col==mu)],Low_random$Digitate_cover[which(Low_random$rate_of_col==mu)],main='Digitate',ylim=c(0,1),las=1,lwd=3,col=rgb(0,0,0,alpha=1),type='l',xlab='event probability',ylab='cover')
  Low_random=super_result[which(super_result$partial_loss==unique(super_result$partial_loss)[2]),]
  lines(Low_random$freq_Rand[which(Low_random$rate_of_col==mu)],Low_random$Digitate_cover[which(Low_random$rate_of_col==mu)],las=1,type='l',lwd=3,col=rgb(0,0,0,alpha=.75))
  Low_random=super_result[which(super_result$partial_loss==unique(super_result$partial_loss)[3]),]
  lines(Low_random$freq_Rand[which(Low_random$rate_of_col==mu)],Low_random$Digitate_cover[which(Low_random$rate_of_col==mu)],las=1,type='l',lwd=3,col=rgb(0,0,0,alpha=.5))
  Low_random=super_result[which(super_result$partial_loss==unique(super_result$partial_loss)[9]),]
  lines(Low_random$freq_Rand[which(Low_random$rate_of_col==mu)],Low_random$Digitate_cover[which(Low_random$rate_of_col==mu)],las=1,type='l',lwd=3,col=rgb(0,0,0,alpha=.1))
  legend(x=0,y=1,c('Ko = 100','Ko = 600','Ko = 1200','Ko = 4500'),bty='n',lty=c(1,1,1,1),lwd=c(2,2,2,2),col=c(rgb(0,0,0,alpha=1),rgb(0,0,0,alpha=.75),rgb(0,0,0,alpha=.25),rgb(0,0,0,alpha=.1)),cex=1) 
  
  Low_random=super_result[which(super_result$partial_loss==unique(super_result$partial_loss)[1]),]
  plot(Low_random$freq_Rand[which(Low_random$rate_of_col==mu)],Low_random$Massive_cover[which(Low_random$rate_of_col==mu)],main='Massive',ylim=c(0,1),las=1,lwd=3,col=rgb(0,0,0,alpha=1),type='l',xlab='event probability',ylab='cover')
  Low_random=super_result[which(super_result$partial_loss==unique(super_result$partial_loss)[2]),]
  lines(Low_random$freq_Rand[which(Low_random$rate_of_col==mu)],Low_random$Massive_cover[which(Low_random$rate_of_col==mu)],las=1,type='l',lwd=3,col=rgb(0,0,0,alpha=.75))
  Low_random=super_result[which(super_result$partial_loss==unique(super_result$partial_loss)[3]),]
  lines(Low_random$freq_Rand[which(Low_random$rate_of_col==mu)],Low_random$Massive_cover[which(Low_random$rate_of_col==mu)],las=1,type='l',lwd=3,col=rgb(0,0,0,alpha=.5))
  Low_random=super_result[which(super_result$partial_loss==unique(super_result$partial_loss)[9]),]
  lines(Low_random$freq_Rand[which(Low_random$rate_of_col==mu)],Low_random$Massive_cover[which(Low_random$rate_of_col==mu)],las=1,type='l',lwd=3,col=rgb(0,0,0,alpha=.1))
  legend(x=0,y=1,c('Ko = 100','Ko = 600','Ko = 1200','Ko = 4500'),bty='n',lty=c(1,1,1,1),lwd=c(2,2,2,2),col=c(rgb(0,0,0,alpha=1),rgb(0,0,0,alpha=.75),rgb(0,0,0,alpha=.25),rgb(0,0,0,alpha=.1)),cex=1) 
  

}
dev.off()
Low_random=super_result[which(super_result$partial_loss==unique(super_result$partial_loss)[2]),]

