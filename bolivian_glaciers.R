
library(simrec)

donnees_cn_1 <- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/transfer_12452611_files_0c8f605b/Bolivia_Holocene_front_elevation/z_front_cn_scen_1.dat", sep = "", header = FALSE)
donnees_cn_2 <- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/transfer_12452611_files_0c8f605b/Bolivia_Holocene_front_elevation/z_front_cn_scen_2.dat", sep = "", header = FALSE)
donnees_zgo_1 <- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/transfer_12452611_files_0c8f605b/Bolivia_Holocene_front_elevation/z_front_zgo_scen_1.dat", sep = "", header = FALSE)
donnees_zgo_3 <- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/transfer_12452611_files_0c8f605b/Bolivia_Holocene_front_elevation/z_front_zgo_scen_3.dat", sep = "", header = FALSE)


cn_1<-as.vector(donnees_cn_1$V1)
l_cn_1<-length(cn_1)
cn_2<-as.vector(donnees_cn_2$V1)
l_cn_2<-length(cn_2)
zgo_1<-as.vector(donnees_zgo_1$V1)
l_zgo_1<-length(zgo_1)
zgo_3<-as.vector(donnees_zgo_3$V1)
l_zgo_3<-length(zgo_3)

plot(cn_1,type='l')
plot(cn_2,type='l')
plot(zgo_1,type='l')
plot(zgo_3,type='l')


plot(rev(cn_1),type='l')
plot(rev(cn_2),type='l')
plot(rev(zgo_1),type='l')
plot(rev(zgo_3),type='l')

### amplitudes
max(cn_1)-min(cn_1)
max(cn_2)-min(cn_2)
max(zgo_1)-min(zgo_1)
max(zgo_3)-min(zgo_3)

### points de départ 
cn_1[1]
cn_2[1]
zgo_1[1]
zgo_3[1]


#### CN_1

pos_NA<--as.numeric(cn_1)
m<-max(pos_NA)
pos_0_NA<-pos_NA
pos_0_NA[which(is.na(pos_0_NA))]<-0
X_NA<-as.numeric(pos_NA)-m
X_0<-X_NA
X_0[which(is.na(X_0))]<-0
l_1_0<-(detect_records_maxima_norm(rev(X_0))$ind_max[-1])[1]
l_ind_0<-detect_records_maxima_norm(rev(X_0))$ind_max[-1]
recordTimes_0=l_ind_0
recordValues_0=(detect_records_maxima_norm(rev(X_0))$rec_max[-1])
dMor_0=cbind(recordTimes_0,recordValues_0)


mor1_0=dMor_0[1,1]
morf_0=dMor_0[length(dMor_0[,1]),1]

years_mor<-(mor1_0:morf_0)[dMor_0[,1]]
year_NA<-c(1:l_cn_1)

par(mfrow=(c(2,1)))
plot((year_NA),-(pos_NA),type='l',xlab='Time (year)',ylab='Cumulative length change (m)',lwd=2)
#plot(rev(X_0),type='l')
points(l_1-dMor_0[,1],dMor_0[,2]+m,pch=19,cex=2,col='blue')
plot((mor1_0:morf_0)-mor1_0,rev(X_NA[mor1_0:morf_0]),type='l',xlab='Time from the last moraine (year)',ylab='Relavtive position (m)',lwd=2,xaxt="n")
ticks <- seq(mor1_0, morf_0, by = 1000)
axis(1, at = ticks, labels = morf_0 - ticks)
points(dMor_0[,1]-mor1_0,dMor_0[,2],pch=19,cex=2,col='blue')
legend('bottomright',legend=c('Moraines','Collected data'),pch=c(19,NA),lwd=c(NA,2),col=c('blue','black'),bty='n')

par(mfrow=(c(2,1)))
plot((year_NA),-(pos_NA),type='l',xlab='Time (year)',ylab='Position (m)',lwd=2)
#plot(rev(X_0),type='l')
#points(l_1-dMor_0[,1],dMor_0[,2]+m,pch=19,cex=2,col='blue')
plot((mor1_0:morf_0)-mor1_0,rev(X_NA[mor1_0:morf_0]),type='l',xlab='Time from the last moraine (year)',ylab='Relavtive position (m)',lwd=2,xaxt="n")
ticks <- seq(mor1_0, morf_0, by = 1000)
axis(1, at = tihcks, labels = morf_0 - ticks)
points(dMor_0[,1]-mor1_0,dMor_0[,2],pch=19,cex=2,col='blue')
legend('bottomright',legend=c('Moraines','Collected data'),pch=c(19,NA),lwd=c(NA,2),col=c('blue','black'),bty='n')

### smooting

breaks <- dMor_0[,1]

# Initialisation
y_smooth <- rev(X_NA)
y<-rev(X_NA)
for(i in 1:(length(breaks)-1)){
  idx <- breaks[i]:breaks[i+1]
  
  # Lissage spline
  fit <- smooth.spline(idx, y[idx], spar = 0.7)
  y_segment <- predict(fit, idx)$y
  
  # Forcer les bornes à être identiques à l'original
  y_segment[1] <- y[idx][1]
  y_segment[length(y_segment)] <- y[idx][length(idx)]
  
  y_smooth[idx] <- y_segment
}

plot((mor1_0:morf_0)-mor1_0,rev(X_NA[mor1_0:morf_0]),col='black',xlab='Time (years)',ylab='Position (m)',type='l')
lines(y_smooth,type='l',col='red',lwd=3)
points(dMor_0[,1]-mor1_0,dMor_0[,2],pch=19,cex=2,col='blue')
legend('bottomright',legend=c('Moraines','Real trajectory','Smoothed trajectory'),pch=c(19,NA,NA),lwd=c(NA,2,3),col=c('blue','black','red'),bty='n')

### Simulations
N<-length((mor1_0:morf_0))
traj_sim <- simRecords(dMor_0,
                       N=N,
                       gaussConv = TRUE,
                       sigmaGauss = 223,
                       sd = 4.5)[,2]

temps<-simRecords(dMor_0,
                  N=N,
                  gaussConv = TRUE,
                  sigmaGauss = 1,
                  sd = 1)[,1]
par(mfrow=(c(1,1)))
plot(temps,traj_sim,type='l',col='red',xlab='time (years)',ylab='Position (m)')
lines((mor1_0:morf_0)-mor1_0,rev(X_NA[mor1_0:morf_0]),col='black')
points(dMor_0[,1]-mor1_0,dMor_0[,2],pch=19,cex=2,col='blue')
legend('bottomright',legend=c('Real trajectory', 'Simulation'),col=c(1,'red'),bty='n',lwd=1)

#### CN_2

pos_NA<--as.numeric(cn_2)
m<-max(pos_NA)
pos_0_NA<-pos_NA
pos_0_NA[which(is.na(pos_0_NA))]<-0
X_NA<-as.numeric(pos_NA)-m
X_0<-X_NA
X_0[which(is.na(X_0))]<-0
l_1_0<-(detect_records_maxima_norm(rev(X_0))$ind_max[-1])[1]
l_ind_0<-detect_records_maxima_norm(rev(X_0))$ind_max[-1]
recordTimes_0=l_ind_0
recordValues_0=(detect_records_maxima_norm(rev(X_0))$rec_max[-1])
dMor_0=cbind(recordTimes_0,recordValues_0)


mor1_0=dMor_0[1,1]
morf_0=dMor_0[length(dMor_0[,1]),1]

years_mor<-(mor1_0:morf_0)[dMor_0[,1]]
year_NA<-c(1:l_cn_2)

par(mfrow=(c(2,1)))
plot((year_NA),-(pos_NA),type='l',xlab='Time (year)',ylab='Position (m)',lwd=2)
#plot(rev(X_0),type='l')
points(l_1-dMor_0[,1],dMor_0[,2]+m,pch=19,cex=2,col='blue')
plot((mor1_0:morf_0)-mor1_0,rev(X_NA[mor1_0:morf_0]),type='l',xlab='Time from the last moraine (year)',ylab='Relavtive position (m)',lwd=2,xaxt="n")
ticks <- seq(mor1_0, morf_0, by = 1000)
axis(1, at = ticks, labels = morf_0 - ticks)
points(dMor_0[,1]-mor1_0,dMor_0[,2],pch=19,cex=2,col='blue')
legend('bottomright',legend=c('Moraines','Collected data'),pch=c(19,NA),lwd=c(NA,2),col=c('blue','black'),bty='n')

### Simulations
N<-length((mor1_0:morf_0))
traj_sim <- simRecords(dMor_0,
                       N=N,
                       gaussConv = TRUE,
                       sigmaGauss = 1,
                       sd = 2.2)[,2]

temps<-simRecords(dMor_0,
                  N=N,
                  gaussConv = TRUE,
                  sigmaGauss = 1,
                  sd = 1)[,1]
par(mfrow=(c(1,1)))
plot(temps,traj_sim,type='l')
lines((mor1_0:morf_0)-mor1_0,rev(X_NA[mor1_0:morf_0]),col='red')
points(dMor_0[,1]-mor1_0,dMor_0[,2],pch=19,cex=2,col='blue')


#### ZGO_1

pos_NA<--as.numeric(zgo_1)
m<-max(pos_NA)
pos_0_NA<-pos_NA
pos_0_NA[which(is.na(pos_0_NA))]<-0
X_NA<-as.numeric(pos_NA)-m
X_0<-X_NA
X_0[which(is.na(X_0))]<-0
l_1_0<-(detect_records_maxima_norm(rev(X_0))$ind_max[-1])[1]
l_ind_0<-detect_records_maxima_norm(rev(X_0))$ind_max[-1]
recordTimes_0=l_ind_0
recordValues_0=(detect_records_maxima_norm(rev(X_0))$rec_max[-1])
dMor_0=cbind(recordTimes_0,recordValues_0)


mor1_0=dMor_0[1,1]
morf_0=dMor_0[length(dMor_0[,1]),1]

years_mor<-(mor1_0:morf_0)[dMor_0[,1]]
year_NA<-c(1:l_zgo_1)

par(mfrow=(c(2,1)))
plot((year_NA),-(pos_NA),type='l',xlab='Time (year)',ylab='Position (m)',lwd=2)
#plot(rev(X_0),type='l')
points(l_1-dMor_0[,1],dMor_0[,2]+m,pch=19,cex=2,col='blue')
plot((mor1_0:morf_0)-mor1_0,rev(X_NA[mor1_0:morf_0]),type='l',xlab='Time from the last moraine (year)',ylab='Relavtive position (m)',lwd=2,xaxt="n")
ticks <- seq(mor1_0, morf_0, by = 1000)
axis(1, at = ticks, labels = morf_0 - ticks)
points(dMor_0[,1]-mor1_0,dMor_0[,2],pch=19,cex=2,col='blue')
legend('bottomright',legend=c('Moraines','Collected data'),pch=c(19,NA),lwd=c(NA,2),col=c('blue','black'),bty='n')

### Simulations
N<-length((mor1_0:morf_0))
traj_sim <- simRecords(dMor_0,
                       N=N,
                       gaussConv = TRUE,
                       sigmaGauss = 10,
                       sd = 0.1)[,2]

temps<-simRecords(dMor_0,
                  N=N,
                  gaussConv = TRUE,
                  sigmaGauss = 1,
                  sd = 1)[,1]
par(mfrow=(c(1,1)))
plot(temps,traj_sim,type='l')
lines((mor1_0:morf_0)-mor1_0,rev(X_NA[mor1_0:morf_0]),col='red',type='l',pch=19,cex=0.2)
points(dMor_0[,1]-mor1_0,dMor_0[,2],pch=19,cex=2,col='blue')


#### ZGO_3

pos_NA<--as.numeric(zgo_3)
m<-max(pos_NA)
pos_0_NA<-pos_NA
pos_0_NA[which(is.na(pos_0_NA))]<-0
X_NA<-as.numeric(pos_NA)-m
X_0<-X_NA
X_0[which(is.na(X_0))]<-0
l_1_0<-(detect_records_maxima_norm(rev(X_0))$ind_max[-1])[1]
l_ind_0<-detect_records_maxima_norm(rev(X_0))$ind_max[-1]
recordTimes_0=l_ind_0
recordValues_0=(detect_records_maxima_norm(rev(X_0))$rec_max[-1])
dMor_0=cbind(recordTimes_0,recordValues_0)


mor1_0=dMor_0[1,1]
morf_0=dMor_0[length(dMor_0[,1]),1]

years_mor<-(mor1_0:morf_0)[dMor_0[,1]]
year_NA<-c(1:l_zgo_3)

par(mfrow=(c(2,1)))
plot((year_NA),-(pos_NA),type='l',xlab='Time (year)',ylab='Position (m)',lwd=2)
#plot(rev(X_0),type='l')
points(l_1-dMor_0[,1],dMor_0[,2]+m,pch=19,cex=2,col='blue')
plot((mor1_0:morf_0)-mor1_0,rev(X_NA[mor1_0:morf_0]),type='l',xlab='Time from the last moraine (year)',ylab='Relavtive position (m)',lwd=2,xaxt="n")
ticks <- seq(mor1_0, morf_0, by = 1000)
axis(1, at = ticks, labels = morf_0 - ticks)
points(dMor_0[,1]-mor1_0,dMor_0[,2],pch=19,cex=2,col='blue')
legend('bottomright',legend=c('Moraines','Collected data'),pch=c(19,NA),lwd=c(NA,2),col=c('blue','black'),bty='n')

### Simulations
N<-length((mor1_0:morf_0))
traj_sim <- simRecords(dMor_0,
                       N=N,
                       gaussConv = TRUE,
                       sigmaGauss = 2,
                       sd = 0.3)[,2]

temps<-simRecords(dMor_0,
                  N=N,
                  gaussConv = TRUE,
                  sigmaGauss = 1,
                  sd = 1)[,1]
par(mfrow=(c(1,1)))
plot(temps,traj_sim,type='l')
lines((mor1_0:morf_0)-mor1_0,rev(X_NA[mor1_0:morf_0]),col='red',type='l',pch=19,cex=0.2)
points(dMor_0[,1]-mor1_0,dMor_0[,2],pch=19,cex=2,col='blue')

### nouvelles données 

LOVECLIM_precip<- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/LOVECLIM_Precip_Adjusted_fullHolocene.dat", sep = "", header = FALSE)
LOVECLIM_temp<- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/LOVECLIM_Temp_Adjusted_fullHolocene.dat", sep = "", header = FALSE)
ZONGO_precip<- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/Zongo_daily_Precip_QM.dat", sep = "", header = FALSE)
ZONGO_precip<- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/Zongo_daily_Temp_QM.dat", sep = "", header = FALSE)

length(LOVECLIM_precip$V1)
length(LOVECLIM_temp$V1)/365
#plot(LOVECLIM_precip$V1,type='p')
length(ZONGO_precip$V1)/74
#plot(ZONGO_precip$V1)

years<-(length(LOVECLIM_temp$V1)/365)
moy_year_LC_p<-matrix(ncol=1,nrow=years)
for(i in 1:years ){
  moy_year_LC_p[i,]<-mean(LOVECLIM_temp$V1[(365*(i-1)+1):(365*i)])
}

plot(moy_year_LC_p,type='l')

y_smooth <- moy_year_LC_p
y<-moy_year_LC_p
smooth <- smooth.spline( y, spar = 0.3)
fit <- predict(smooth,1:(length(y)))$y

y_smooth <- cn_1
y<-cn_1
smooth <- smooth.spline( y, spar = 0.3)
fit2 <- predict(smooth,1:(length(y)))$y

y_smooth <- cn_2[-c(1:130)]
y<-cn_2[-c(1:130)]
smooth <- smooth.spline( y, spar = 0.3)
fit3 <- predict(smooth,1:(length(y)))$y
plot(-fit3)
plot(-rev(fit3))
plot(fit3)

y_smooth <- zgo_1
y<-zgo_1
smooth <- smooth.spline( y, spar = 0.3)
fit4 <- predict(smooth,1:(length(y)))$y

y_smooth <- zgo_3
y<-zgo_3
smooth <- smooth.spline( y, spar = 0.3)
fit5<- predict(smooth,1:(length(y)))$y

plot(scale(moy_year_LC_p),type='l',col='grey')
lines(scale(fit2),type='l',col='blue',lwd=3)
lines((1:length(fit3))+131,scale(fit3),type='l',col='orange',lwd=3)
lines(scale(fit4),type='l',col='green',lwd=3)
lines(scale(fit5),type='l',col='pink',lwd=3)
lines(scale(fit),col='red',type='l',lwd=3)

### pareil avec precip

years<-(length(LOVECLIM_precip$V1)/365)
moy_year_LC_pr<-matrix(ncol=1,nrow=years)
for(i in 1:years ){
  moy_year_LC_pr[i,]<-mean(LOVECLIM_precip$V1[(365*(i-1)+1):(365*i)])
}

plot(scale(moy_year_LC_pr),type='l')
lines(scale(moy_year_LC_p),type='l',col='red')

y_smooth <- moy_year_LC_pr
y<-moy_year_LC_pr
smooth <- smooth.spline( y, spar = 0.3)
fit6 <- predict(smooth,1:(length(y)))$y


plot(scale(moy_year_LC_pr),type='l',col='#BBEEFC',xlab='Time (years)',ylab='Scaled and smoothed variations')
lines(scale(moy_year_LC_p),type='l',col='#FCC6BB')
lines(scale(fit6),col='#0998C8',type='l',lwd=4,lty=1)
lines(scale(fit),col='#F87C63',type='l',lwd=4,lty=1)
lines(scale(fit2),type='l',col='#034424',lwd=2)
lines(scale(fit3),type='l',col='#05703B',lwd=2)
lines(scale(fit4),type='l',col='#079C52',lwd=2)
lines(scale(fit5),type='l',col='#09C869',lwd=2)
legend("bottomright", legend = c('Temperatures','Precipitations','cn1','cn2','zgo1','zgo3'),col=c('#0998C8','#F87C63','#034424','#05703B','#079C52','#09C869'),lwd=c(4,4,2,2,2,2),bty='n')

plot(scale(moy_year_LC_pr),type='l',col='#BBEEFC',xlab='Time (years)',ylab='Scaled and smoothed variations')
lines(scale(fit6),col='#0998C8',type='l',lwd=4,lty=1)
lines(scale(fit2),type='l',col='#034424',lwd=2)
lines(scale(fit3),type='l',col='#05703B',lwd=2)
lines(scale(fit4),type='l',col='#079C52',lwd=2)
lines(scale(fit5),type='l',col='#09C869',lwd=2)
legend("bottomright", legend = c('Precipitations','cn1','cn2','zgo1','zgo3'),col=c('#0998C8','#034424','#05703B','#079C52','#09C869'),lwd=c(4,2,2,2,2),bty='n')


plot(scale(moy_year_LC_p),type='l',col='#FCC6BB',xlab='Time (years)',ylab='Scaled and smoothed variations')
lines(scale(fit),col='#F87C63',type='l',lwd=4,lty=1)
lines(scale(fit2),type='l',col='#034424',lwd=2)
lines(scale(fit3),type='l',col='#05703B',lwd=2)
lines(scale(fit4),type='l',col='#079C52',lwd=2)
lines(scale(fit5),type='l',col='#09C869',lwd=2)
legend("bottomright", legend = c('Temperatures','cn1','cn2','zgo1','zgo3'),col=c('#F87C63','#034424','#05703B','#079C52','#09C869'),lwd=c(4,2,2,2,2),bty='n')

### Moraines historiques
ages<-c(2021-1679,2021-1727,2021-1771,2021-1804,2021-1834,2021-1860,10100)
incer<-c(10,9,7,9,8,7,400)

ylim <- par("usr")[3:4]

plot(scale(moy_year_LC_pr),type='l',col='#BBEEFC',xlab='Time (years)',ylab='Scaled and smoothed variations')
lines(scale(moy_year_LC_p),type='l',col='#FCC6BB')
lines(scale(fit6),col='#0998C8',type='l',lwd=4,lty=1)
lines(scale(fit),col='#F87C63',type='l',lwd=4,lty=1)
lines(scale(fit2),type='l',col='#034424',lwd=2)
lines(scale(fit3),type='l',col='#05703B',lwd=2)
lines(scale(fit4),type='l',col='#079C52',lwd=2)
lines(scale(fit5),type='l',col='#09C869',lwd=2)
for(i in 1:(length(ages))){
  abline(v=ages[i],lty=2,col='red')
  rect(
    xleft  = ages[i] - incer[i],
    xright = ages[i] + incer[i],
    ybottom = ylim[1],
    ytop    = ylim[2],
    col = rgb(1, 0, 0, 0.2),  
    border = NA
  )
}
legend("bottomright", legend = c('Temperatures','Precipitations','cn1','cn2','zgo1','zgo3'),col=c('#0998C8','#F87C63','#034424','#05703B','#079C52','#09C869'),lwd=c(4,4,2,2,2,2),bty='n')



#cn2
pos_NA<--as.numeric(fit3)
m<-max(pos_NA)
pos_0_NA<-pos_NA
pos_0_NA[which(is.na(pos_0_NA))]<-0
X_NA<-as.numeric(pos_NA)-m
X_0_cn2<-X_NA
X_0_cn2[which(is.na(X_0_cn2))]<-0
l_1_0<-(detect_records_maxima_norm(rev(X_0_cn2))$ind_max[-1])[1]
l_ind_0<-detect_records_maxima_norm(rev(X_0_cn2))$ind_max[-1]
recordTimes_0_cn2=l_ind_0
recordValues_0_cn2=(detect_records_maxima_norm(rev(X_0_cn2))$rec_max[-1])
dMor_0_cn2=cbind(recordTimes_0_cn2,recordValues_0_cn2)

mor1_0=dMor_0_cn2[1,1]
morf_0=dMor_0_cn2[length(dMor_0_cn2[,1]),1]

years_mor<-(mor1_0:morf_0)[dMor_0_cn2[,1]]
year_NA<-c(1:length(fit5))

plot(rev(X_0_cn2),type='l',xlab='Time from the last moraine (year)',ylab='Relavtive position (m)',lwd=2,xaxt="n")
ticks <- seq(mor1_0, morf_0, by = 1000)
axis(1, at = ticks, labels = morf_0 - ticks)
points(dMor_0_cn2[,1],dMor_0_cn2[,2],pch=19,cex=2,col='blue')
legend('bottomright',legend=c('Moraines','Collected data'),pch=c(19,NA),lwd=c(NA,2),col=c('blue','black'),bty='n')

#zgo3
pos_NA<--as.numeric(fit5)
m<-max(pos_NA)
pos_0_NA<-pos_NA
pos_0_NA[which(is.na(pos_0_NA))]<-0
X_NA<-as.numeric(pos_NA)-m
X_0_zgo3<-X_NA
X_0_zgo3[which(is.na(X_0_zgo3))]<-0
l_1_0<-(detect_records_maxima_norm(rev(X_0_zgo3))$ind_max[-1])[1]
l_ind_0<-detect_records_maxima_norm(rev(X_0_zgo3))$ind_max[-1]
recordTimes_0_zgo3=l_ind_0
recordValues_0_zgo3=(detect_records_maxima_norm(rev(X_0_zgo3))$rec_max[-1])
dMor_0_zgo3=cbind(recordTimes_0_zgo3,recordValues_0_zgo3)

mor1_0=dMor_0_zgo3[1,1]
morf_0=dMor_0_zgo3[length(dMor_0_zgo3[,1]),1]

years_mor<-(mor1_0:morf_0)[dMor_0_zgo3[,1]]
year_NA<-c(1:length(fit5))

plot(rev(X_0_zgo3),type='l',xlab='Time from the last moraine (year)',ylab='Relavtive position (m)',lwd=2,xaxt="n")
ticks <- seq(mor1_0, morf_0, by = 1000)
axis(1, at = ticks, labels = morf_0 - ticks)
points(dMor_0_zgo3[,1],dMor_0_zgo3[,2],pch=19,cex=2,col='blue')
legend('bottomright',legend=c('Moraines','Collected data'),pch=c(19,NA),lwd=c(NA,2),col=c('blue','black'),bty='n')

l_1_0_cn1<-(detect_records_maxima_norm(rev(fit2))$ind_max[-1])[1]
l_ind_0_cn1<-detect_records_maxima_norm(rev(fit2))$ind_max[-1]
recordTimes_0_cn1=l_ind_0_cn1
recordValues_0=(detect_records_maxima_norm(rev(fit2))$rec_max[-1])
dMor_0_cn1=cbind(recordTimes_0_cn1,recordValues_0)

l_1_0_cn2<-(detect_records_maxima_norm(fit3[-(1:130)])$ind_max[-1])[1]
l_ind_0_cn2<-detect_records_maxima_norm(fit3[-(1:130)])$ind_max[-1]
recordTimes_0_cn2=l_ind_0_cn2+260
recordValues_0=(detect_records_maxima_norm(fit3[-(1:130)])$rec_max[-1])
dMor_0_cn2=cbind(recordTimes_0_cn2,recordValues_0)

l_1_0_zgo1<-(detect_records_maxima_norm(fit4)$ind_max[-1])[1]
l_ind_0_zgo1<-detect_records_maxima_norm(fit4)$ind_max[-1]
recordTimes_0_zgo1=l_ind_0_zgo1
recordValues_0=(detect_records_maxima_norm(fit4)$rec_max[-1])
dMor_0_zgo1=cbind(recordTimes_0_zgo1,recordValues_0)

l_1_0_zgo3<-(detect_records_maxima_norm(fit5)$ind_max[-1])[1]
l_ind_0_zgo3<-detect_records_maxima_norm(fit5)$ind_max[-1]
recordTimes_0_zgo3=l_ind_0_zgo3
recordValues_0=(detect_records_maxima_norm(fit5)$rec_max[-1])
dMor_0_zgo3=cbind(recordTimes_0_zgo3,recordValues_0)

times<-cbind(recordTimes_0_cn1,recordTimes_0_cn2,recordTimes_0_zgo1,recordTimes_0_zgo3)

times<-cbind(dMor_0_cn2[,1],dMor_0_zgo3[,1])
col_gla<-c('#034424','#05703B','#079C52','#09C869')

plot((scale(moy_year_LC_pr)),type='l',col='#BBEEFC',xlab='Time (years)',ylab='Scaled and smoothed variations')
lines((scale(moy_year_LC_p)),type='l',col='#FCC6BB')
lines((scale(fit6)),col='#0998C8',type='l',lwd=2,lty=1)
lines((scale(fit)),col='#F87C63',type='l',lwd=2,lty=1)
lines(scale(fit2),type='l',col='#034424',lwd=4)
lines((1:length(fit3))+131,scale(fit3),type='l',col='#05703B',lwd=4)
lines(scale(fit4),type='l',col='#079C52',lwd=4)
lines(scale(fit5),type='l',col='#09C869',lwd=4)
for(i in 1:(length(ages))){
  abline(v=length(fit6)-ages[i],lty=2,col='red')
  rect(
    xleft  = length(fit6)-ages[i] - incer[i],
    xright = length(fit6)-ages[i] + incer[i],
    ybottom = ylim[1],
    ytop    = ylim[2],
    col = rgb(1, 0, 0, 0.2),  
    border = NA
  )
}
for(i in 1:length(times[,1])){
  for(j in 1:length(times[1,])){
    abline(v=times[i,j],col=col_gla[j],lty=2)
  }
}
legend("bottomright", legend = c('Temperatures','Precipitations','cn1','cn2','zgo1','zgo3','Detected moraines','Observed moraines'),col=c('#0998C8','#F87C63','#034424','#05703B','#079C52','#09C869','#05703B','red'),lwd=c(2,2,4,4,4,4,2,2),lty=c(1,1,1,1,1,1,2,2))



plot((1:length(fit3))+131,scale(fit3),type='l',col='#05703B',lwd=4)
for(i in 1:length(times[,1])){
  for(j in c(2)){
    abline(v=length(fit3)-times[i,j],col=col_gla[j],lty=2)
  }
}


plot((scale(moy_year_LC_pr)),type='l',col='#BBEEFC',xlab='Time (years)',ylab='Scaled and smoothed variations')
lines((scale(moy_year_LC_p)),type='l',col='#FCC6BB')
lines((scale(fit6)),col='#0998C8',type='l',lwd=2,lty=1)
lines((scale(fit)),col='#F87C63',type='l',lwd=2,lty=1)
lines((1:length(fit3))+131,rev(-scale(fit3)),type='l',col='#05703B',lwd=4)
lines(rev(-scale(fit5)),type='l',col='#09C869',lwd=4)
for(i in 1:(length(ages))){
  abline(v=ages[i],lty=2,col='red')
  rect(
    xleft  = ages[i] - incer[i],
    xright = ages[i] + incer[i],
    ybottom = ylim[1],
    ytop    = ylim[2],
    col = rgb(1, 0, 0, 0.2),  
    border = NA
  )
}
for(i in 1:length(times[,1])){
  for(j in 1:length(times[1,])){
    abline(v=length(fit5)-times[i,j],col=col_gla[j],lty=2)
  }
}
legend("topleft", legend = c('Climate :','Temperatures','Precipitations'),col=c(NA,'#0998C8','#F87C63'),lwd=c(2,2),lty=c(1,1))
legend("top", legend = c('Glaciers and moraines :','cn2','zgo3','Detected moraines','Observed moraines'),col=c(NA,'#05703B','#09C869','#05703B','red'),lwd=c(1,4,4,2,2),lty=c(1,1,1,2,2))


##### BONNE REPRESENTATION #####
#cn2
pos_NA<-rev(-as.numeric(fit3))
plot(pos_NA)
m<-max(pos_NA)
pos_0_NA<-pos_NA
pos_0_NA[which(is.na(pos_0_NA))]<-0
X_NA<-as.numeric(pos_NA)-m
X_0_cn2<-X_NA
X_0_cn2[which(is.na(X_0_cn2))]<-0
l_1_0<-(detect_records_maxima_norm(X_0_cn2)$ind_max[-1])[1]
l_ind_0<-detect_records_maxima_norm(X_0_cn2)$ind_max[-1]
recordTimes_0_cn2=l_ind_0
recordValues_0_cn2=(detect_records_maxima_norm(X_0_cn2)$rec_max[-1])
dMor_0_cn2=cbind(recordTimes_0_cn2,recordValues_0_cn2)

mor1_0=dMor_0_cn2[1,1]
morf_0=dMor_0_cn2[length(dMor_0_cn2[,1]),1]

years_mor<-(mor1_0:morf_0)[dMor_0_cn2[,1]]
year_NA<-c(1:length(fit5))

plot(X_0_cn2,type='l',xlab='Time from the last moraine (year)',ylab='Relavtive position (m)',lwd=2,xaxt="n")
ticks <- seq(mor1_0, morf_0, by = 1000)
axis(1, at = ticks, labels = morf_0 - ticks)
points(dMor_0_cn2[,1],dMor_0_cn2[,2],pch=19,cex=2,col='blue')
legend('bottomright',legend=c('Moraines','Collected data'),pch=c(19,NA),lwd=c(NA,2),col=c('blue','black'),bty='n')

#zgo3
pos_NA<-rev(-as.numeric(fit5))
m<-max(pos_NA)
pos_0_NA<-pos_NA
pos_0_NA[which(is.na(pos_0_NA))]<-0
X_NA<-as.numeric(pos_NA)-m
X_0_zgo3<-X_NA
X_0_zgo3[which(is.na(X_0_zgo3))]<-0
l_1_0<-(detect_records_maxima_norm((X_0_zgo3))$ind_max[-1])[1]
l_ind_0<-detect_records_maxima_norm((X_0_zgo3))$ind_max[-1]
recordTimes_0_zgo3=l_ind_0
recordValues_0_zgo3=(detect_records_maxima_norm((X_0_zgo3))$rec_max[-1])
dMor_0_zgo3=cbind(recordTimes_0_zgo3,recordValues_0_zgo3)

mor1_0=dMor_0_zgo3[1,1]
morf_0=dMor_0_zgo3[length(dMor_0_zgo3[,1]),1]

years_mor<-(mor1_0:morf_0)[dMor_0_zgo3[,1]]
year_NA<-c(1:length(fit5))

plot(X_0_zgo3,type='l',xlab='Time from the last moraine (year)',ylab='Relavtive position (m)',lwd=2,xaxt="n")
ticks <- seq(mor1_0, morf_0, by = 1000)
axis(1, at = ticks, labels = morf_0 - ticks)
points(dMor_0_zgo3[,1],dMor_0_zgo3[,2],pch=19,cex=2,col='blue')
legend('bottomright',legend=c('Moraines','Collected data'),pch=c(19,NA),lwd=c(NA,2),col=c('blue','black'),bty='n')

l_1_0_cn1<-(detect_records_maxima_norm(rev(fit2))$ind_max[-1])[1]
l_ind_0_cn1<-detect_records_maxima_norm(rev(fit2))$ind_max[-1]
recordTimes_0_cn1=l_ind_0_cn1
recordValues_0=(detect_records_maxima_norm(rev(fit2))$rec_max[-1])
dMor_0_cn1=cbind(recordTimes_0_cn1,recordValues_0)

l_1_0_cn2<-(detect_records_maxima_norm(fit3[-(1:130)])$ind_max[-1])[1]
l_ind_0_cn2<-detect_records_maxima_norm(fit3[-(1:130)])$ind_max[-1]
recordTimes_0_cn2=l_ind_0_cn2+260
recordValues_0=(detect_records_maxima_norm(fit3[-(1:130)])$rec_max[-1])
dMor_0_cn2=cbind(recordTimes_0_cn2,recordValues_0)

l_1_0_zgo1<-(detect_records_maxima_norm(fit4)$ind_max[-1])[1]
l_ind_0_zgo1<-detect_records_maxima_norm(fit4)$ind_max[-1]
recordTimes_0_zgo1=l_ind_0_zgo1
recordValues_0=(detect_records_maxima_norm(fit4)$rec_max[-1])
dMor_0_zgo1=cbind(recordTimes_0_zgo1,recordValues_0)

l_1_0_zgo3<-(detect_records_maxima_norm(fit5)$ind_max[-1])[1]
l_ind_0_zgo3<-detect_records_maxima_norm(fit5)$ind_max[-1]
recordTimes_0_zgo3=l_ind_0_zgo3
recordValues_0=(detect_records_maxima_norm(fit5)$rec_max[-1])
dMor_0_zgo3=cbind(recordTimes_0_zgo3,recordValues_0)

times<-cbind(recordTimes_0_cn1,recordTimes_0_cn2,recordTimes_0_zgo1,recordTimes_0_zgo3)

times<-cbind(dMor_0_cn2[,1],dMor_0_zgo3[,1])
col_gla<-c('#034424','#05703B','#079C52','#09C869')

plot((scale(moy_year_LC_pr)),type='l',col='#BBEEFC',xlab='Time (years)',ylab='Scaled and smoothed variations')
lines((scale(moy_year_LC_p)),type='l',col='#FCC6BB')
lines((scale(fit6)),col='#0998C8',type='l',lwd=2,lty=1)
lines((scale(fit)),col='#F87C63',type='l',lwd=2,lty=1)
lines(scale(fit2),type='l',col='#034424',lwd=4)
lines((1:length(fit3))+131,scale(fit3),type='l',col='#05703B',lwd=4)
lines(scale(fit4),type='l',col='#079C52',lwd=4)
lines(scale(fit5),type='l',col='#09C869',lwd=4)
for(i in 1:(length(ages))){
  abline(v=length(fit6)-ages[i],lty=2,col='red')
  rect(
    xleft  = length(fit6)-ages[i] - incer[i],
    xright = length(fit6)-ages[i] + incer[i],
    ybottom = ylim[1],
    ytop    = ylim[2],
    col = rgb(1, 0, 0, 0.2),  
    border = NA
  )
}
for(i in 1:length(times[,1])){
  for(j in 1:length(times[1,])){
    abline(v=times[i,j],col=col_gla[j],lty=2)
  }
}
legend("bottomright", legend = c('Temperatures','Precipitations','cn1','cn2','zgo1','zgo3','Detected moraines','Observed moraines'),col=c('#0998C8','#F87C63','#034424','#05703B','#079C52','#09C869','#05703B','red'),lwd=c(2,2,4,4,4,4,2,2),lty=c(1,1,1,1,1,1,2,2))



plot((1:length(fit3))+131,scale(fit3),type='l',col='#05703B',lwd=4)
for(i in 1:length(times[,1])){
  for(j in c(2)){
    abline(v=length(fit3)-times[i,j],col=col_gla[j],lty=2)
  }
}


plot((scale(moy_year_LC_pr)),type='l',col='#BBEEFC',xaxt="n",xlab='Age (years)',ylab='Scaled and smoothed variations')
ticks <- seq(0, 12000, by = 1000)
axis(1, at = ticks, labels = 12000 - ticks)
lines((scale(moy_year_LC_p)),type='l',col='#FCC6BB')
lines((scale(fit6)),col='#0998C8',type='l',lwd=2,lty=1)
lines((scale(fit)),col='#F87C63',type='l',lwd=2,lty=1)
#lines((1:length(fit3))+131,rev(-scale(fit3)),type='l',col='#05703B',lwd=4)
lines(rev(-scale(fit3)),type='l',col='#05703B',lwd=4)
lines(rev(-scale(fit5)),type='l',col='#09C869',lwd=4)
for(i in 1:(length(ages))){
  abline(v=length(fit5)-ages[i],lty=2,col='red')
  rect(
    xleft  = length(fit5)-ages[i] - incer[i],
    xright = length(fit5)-ages[i] + incer[i],
    ybottom = ylim[1],
    ytop    = ylim[2],
    col = rgb(1, 0, 0, 0.2),  
    border = NA
  )
}
for(i in 1:length(times[,1])){
  for(j in 1:length(times[1,])){
    abline(v=times[i,j],col=col_gla[j],lty=2)
  }
}
legend("bottom", legend = c('Climate :','Temperatures','Precipitations'),col=c(NA,'#0998C8','#F87C63'),lwd=c(2,2),lty=c(1,1))
legend("top", legend = c('Glaciers and moraines :','cn2','zgo3','Detected moraines','Observed moraines'),col=c(NA,'#05703B','#09C869','#05703B','red'),lwd=c(1,4,4,2,2),lty=c(1,1,1,2,2))

