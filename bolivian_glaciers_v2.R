library(simrec)

# variations de front
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

# données climatiques
LOVECLIM_precip<- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/LOVECLIM_Precip_Adjusted_fullHolocene.dat", sep = "", header = FALSE)
LOVECLIM_temp<- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/LOVECLIM_Temp_Adjusted_fullHolocene.dat", sep = "", header = FALSE)
ZONGO_precip<- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/Zongo_daily_Precip_QM.dat", sep = "", header = FALSE)
ZONGO_precip<- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/Zongo_daily_Temp_QM.dat", sep = "", header = FALSE)

# moyennes annuelles
years<-(length(LOVECLIM_temp$V1)/365)
moy_year_LC_p<-matrix(ncol=1,nrow=years)
for(i in 1:years ){
  moy_year_LC_p[i,]<-mean(LOVECLIM_temp$V1[(365*(i-1)+1):(365*i)])
}

years<-(length(LOVECLIM_precip$V1)/365)
moy_year_LC_pr<-matrix(ncol=1,nrow=years)
for(i in 1:years ){
  moy_year_LC_pr[i,]<-mean(LOVECLIM_precip$V1[(365*(i-1)+1):(365*i)])
}


plot(scale(moy_year_LC_pr),type='l')
lines(scale(moy_year_LC_p),type='l',col='red')

# lissages
y_smooth <- moy_year_LC_p
y<-moy_year_LC_p
smooth <- smooth.spline( y, spar = 0.3)
fit <- predict(smooth,1:(length(y)))$y

y_smooth <- cn_2
y<-cn_2
smooth <- smooth.spline( y, spar = 0.3)
fit3 <- predict(smooth,1:(length(y)))$y

y_smooth <- zgo_3
y<-zgo_3
smooth <- smooth.spline( y, spar = 0.3)
fit5<- predict(smooth,1:(length(y)))$y

y_smooth <- moy_year_LC_pr
y<-moy_year_LC_pr
smooth <- smooth.spline( y, spar = 0.3)
fit6 <- predict(smooth,1:(length(y)))$y


plot(moy_year_LC_pr,type='l',col='#BBEEFC',xaxt="n",xlab='Age (years)',ylab='Precipitations')
ticks <- c(seq(0, 11700, by = 1000),11700)
axis(1, at = ticks, labels = 11700 - ticks)
lines(fit6,col='#0998C8')

plot(moy_year_LC_p,type='l',col='#FCC6BB',xaxt="n",xlab='Age (years)',ylab='Temperatures')
ticks <- c(seq(0, 11700, by = 1000),11700)
axis(1, at = ticks, labels = 11700 - ticks)
lines(fit,col='#F87C63')

# moraines detection
#cn2
pos_NA<-rev(-as.numeric(fit3))
#plot(pos_NA)
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


times<-cbind(dMor_0_cn2[,1],dMor_0_zgo3[,1])
col_gla<-c('#034424','#05703B','#079C52','#09C869')


plot((1:length(fit3))+131,scale(fit3),type='l',col='#05703B',lwd=4)
for(i in 1:length(times[,1])){
  for(j in c(2)){
    abline(v=length(fit3)-times[i,j],col=col_gla[j],lty=2)
  }
}

ages<-c(2021-1679,2021-1727,2021-1771,2021-1804,2021-1834,2021-1860,10100)
incer<-c(10,9,7,9,8,7,400)

plot((scale(moy_year_LC_pr)),type='l',col='#BBEEFC',xaxt="n",xlab='Age (years)',ylab='Scaled and smoothed variations')
ticks <- c(seq(0, 11700, by = 1000),11700)
axis(1, at = ticks, labels = 11700 - ticks)
lines((scale(moy_year_LC_p)),type='l',col='#FCC6BB')
lines((scale(fit6)),col='#0998C8',type='l',lwd=2,lty=1)
lines((scale(fit)),col='#F87C63',type='l',lwd=2,lty=1)
#lines((1:length(fit3))+131,rev(-scale(fit3)),type='l',col='#05703B',lwd=4)
#lines(rev(-scale(fit3)),type='l',col='#05703B',lwd=4)
#lines(rev(-scale(fit5)),type='l',col='#09C869',lwd=4)
lines(scale(fit3),type='l',col='#05703B',lwd=4)
lines(scale(fit5),type='l',col='#09C869',lwd=4)
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
    abline(v=length(fit5)-times[i,j],col=col_gla[j],lty=2)
  }
}
abline(h=0,col=1)
legend("bottom", legend = c('Climate :','Temperatures','Precipitations'),col=c(NA,'#0998C8','#F87C63'),lwd=c(2,2),lty=c(1,1))
legend("top", legend = c('Glaciers and moraines :','cn2','zgo3','Detected moraines','Observed moraines'),col=c(NA,'#05703B','#09C869','#05703B','red'),lwd=c(1,4,4,2,2),lty=c(1,1,1,2,2))

### graphiques presentation irimont
plot(scale(fit5),type='l',col='#09C869',lwd=4)
for(i in 1:length(times[,1])){
  for(j in c(2)){
    abline(v=length(fit5)-times[i,j],col=col_gla[j],lty=2)
  }
}

###### Zoom sur les moraines les plus récentes

ages<-c(2021-1679,2021-1727,2021-1771,2021-1804,2021-1834,2021-1860)
incer<-c(10,9,7,9,8,7)

cn2_zoom<-cn_2[(l_cn_2-600):l_cn_2]
plot(cn2_zoom,type='l')

# lissage
y_smooth <- cn2_zoom
y<-cn2_zoom
smooth <- smooth.spline( y, spar = 0.4)
fit_cn2_zoom <- predict(smooth,1:(length(y)))$y

plot(cn2_zoom,type='l')
lines(fit_cn2_zoom,col='#05703B',lwd=4)

# moraines
pos_NA<-rev(-as.numeric(fit_cn2_zoom))
m<-max(pos_NA)
pos_0_NA<-pos_NA
pos_0_NA[which(is.na(pos_0_NA))]<-0
X_NA<-as.numeric(pos_NA)-m
X_0_c2z<-X_NA
X_0_c2z[which(is.na(X_0_c2z))]<-0
l_1_0<-(detect_records_maxima_norm((X_0_c2z))$ind_max[-1])[1]
l_ind_0<-detect_records_maxima_norm((X_0_c2z))$ind_max[-1]
recordTimes_0_c2z=l_ind_0
recordValues_0_c2z=(detect_records_maxima_norm((X_0_c2z))$rec_max[-1])
dMor_0_c2z=cbind(recordTimes_0_c2z,recordValues_0_c2z)

mor1_0=dMor_0_c2z[1,1]
morf_0=dMor_0_c2z[length(dMor_0_c2z[,1]),1]

years_mor<-(mor1_0:morf_0)[dMor_0_c2z[,1]]
year_NA<-c(1:length(fit_cn2_zoom))

times<-dMor_0_c2z

plot(X_0_c2z,type='l',xlab='Time from the last moraine (year)',ylab='Relavtive position (m)',lwd=2,xaxt="n")
ticks <- seq(1,600, by = 10)
axis(1, at = ticks, labels = 2021-ticks)
points(dMor_0_c2z[,1],dMor_0_c2z[,2],pch=19,cex=2,col='blue')
legend('bottomright',legend=c('Moraines','Collected data'),pch=c(19,NA),lwd=c(NA,2),col=c('blue','black'),bty='n')

plot(cn2_zoom,type='l')
lines(fit_cn2_zoom,col='#05703B',lwd=4)
for(i in 1:(length(ages))){
  abline(v=length(fit_cn2_zoom)-ages[i],lty=2,col='red')
}
for(i in 1:length(times[,1])){
    abline(v=length(fit_cn2_zoom)-times[i,1],col=col_gla[j],lty=2)
}


##### test moraines manquantes

y_smooth <- zgo_3
y<-zgo_3
smooth <- smooth.spline( y, spar = 0.4)
fit5<- predict(smooth,1:(length(y)))$y


pos_NA<-rev(-as.numeric(fit5))
m<-min(pos_NA)
pos_0_NA<-pos_NA-m
pos_0_NA[which(is.na(pos_0_NA))]<-0
X_NA<-as.numeric(pos_0_NA)
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

plot((mor1_0:morf_0),X_0_zgo3[(mor1_0:morf_0)],type='l',xlab='Time (year)',ylab='Position (m)',lwd=2,xaxt="n")
ticks <- seq(mor1_0, morf_0, by = 1000)
axis(1, at = ticks, labels = morf_0 - ticks)
points(dMor_0_zgo3[,1],dMor_0_zgo3[,2],pch=19,cex=2,col='blue')
legend('bottomright',legend=c('Moraines','Smoothed Zongo front variation'),pch=c(19,NA),lwd=c(NA,2),col=c('blue','black'),bty='n')



traj_sim <-simRecords(dMor_0_zgo3,
                      N=N,
                      gaussConv = TRUE,
                      sigmaGauss = 100,
                      sd = 0.18)[,2]
temps<-simRecords(dMor_0_zgo3,
                  N=N,
                  gaussConv = TRUE,
                  sigmaGauss = 18,
                  sd = 0.18)[,1]

plot((mor1_0:morf_0),X_0_zgo3[(mor1_0:morf_0)],type='l',xlab='Time from the last moraine (year)',ylab='Relavtive position (m)',lwd=2,xaxt="n")
lines(temps,traj_sim,type='l',col='red')
points(dMor_0_zgo3[,1],dMor_0_zgo3[,2],pch=19,cex=2,col='blue')

nm<-length(dMor_0_zgo3[,1])
iter<-1000
X_miss_mor <- matrix(NA, nrow = iter*(nm-1), ncol = N) 
dMor2<-rbind(dMor_0_zgo3[1,],dMor_0_zgo3)


for(i in 1:(nm-1)){
  dMor2<-dMor2[-2,]
  for(j in 1:iter){
    traj_sim <-simRecords(dMor2,
                          N=N,
                          gaussConv = TRUE,
                          sigmaGauss = 100,
                          sd = 0.18)[,2]
    X_miss_mor[(i-1)*iter+j, ] <- traj_sim
  }
}


ts_interp1<-X_0_zgo3[as.integer(temps)]
to_comp<-ts_interp1


mse <- function(a,b) mean((a-b)^2)
MAPE <- function(a, b) {
  mean(abs(a - b) / abs(b), na.rm = TRUE)
}
w<- which(!is.na((to_comp)))

err_pred <- apply(X_miss_mor[,w], 1,function(x) mse(to_comp[w]  , x))

w1<-which(err_pred==min(err_pred))

err_pred_MAPE <- apply(X_miss_mor[,w],1, function(x) MAPE(to_comp[w], x))


plot(err_pred)

# moyennes d'erreurs 
mean_err<-c()

for(i in  1:(nm-1)){
  e<-mean(err_pred[((i-1)*iter):(i*iter)])
  mean_err<-c(mean_err,e)
}

# évolution de la mse selon le nombre de moraines enlevées
plot(mean_err,type='b',xlab='Number of missing moraines',ylab='Mean of the MSE ')

# enveloppes



cols_s<- c(
  "#264653", # bleu pétrole
  "#2A9D8F", # turquoise
  "#8AB17D", # vert sauge
  "#E9C46A", # jaune sable
  "#F4A261", # orange doux
  "#E76F51", # corail
  "#D62828", # rouge profond
  "#6A4C93", # violet
  "#457B9D", # bleu acier
  "#A8DADC"  # bleu pastel
)

library(scales)

plot((mor1_0:morf_0),X_0_zgo3[(mor1_0:morf_0)],type='l',lwd=1,ylim=c(-15,17),xlab='Time (year)',ylab='Position (m)')
for(i in rev(1:(nm-1))){
  q5_1 <- apply(X_miss_mor[((i-1)*iter):(i*iter),], 2, quantile, probs = 0.05)
  q95_1 <- apply(X_miss_mor[((i-1)*iter):(i*iter),], 2, quantile, probs = 0.95)
  polygon(c(temps, rev(temps)), c(q5_1, rev(q95_1)),
          col = alpha(cols_s[i+1],0.6), border = NA)
}
lines((mor1_0:morf_0),X_0_zgo3[(mor1_0:morf_0)],type='l',lwd=4)
points(dMor_0_zgo3[,1],dMor_0_zgo3[,2],pch=19,cex=2,col='blue')


# graphique combiné 
par(mfrow=c(3,1))

# variations
plot((mor1_0:morf_0),X_0_zgo3[(mor1_0:morf_0)],type='l',xlab='Time (year)',ylab='Position to the top(m)',lwd=2,xaxt="n",main='Mathematical moraines detected on the smoothed Zongo front variation')
ticks <- seq(mor1_0, morf_0, by = 1000)
axis(1, at = ticks, labels = ticks)
points(dMor_0_zgo3[,1],dMor_0_zgo3[,2],pch=19,cex=2,col='blue')
legend('bottomright',legend=c('Moraines','Smoothed Zongo front variation'),pch=c(19,NA),lwd=c(NA,2),col=c('blue','black'),bty='n')


# enveloppes simulations
plot((mor1_0:morf_0),X_0_zgo3[(mor1_0:morf_0)],type='l',lwd=1,ylim=c(-15,17),xaxt='n',xlab='Time (year)',ylab='Position to the top (m)',main='90% envelops of the brownian simulations depending on the number of missing moraines')
for(i in rev(1:(nm-1))){
  q5_1 <- apply(X_miss_mor[((i-1)*iter):(i*iter),], 2, quantile, probs = 0.05)
  q95_1 <- apply(X_miss_mor[((i-1)*iter):(i*iter),], 2, quantile, probs = 0.95)
  polygon(c(temps, rev(temps)), c(q5_1, rev(q95_1)),
          col = alpha(cols_s[i+1],0.6), border = NA)
}
ticks <- seq(mor1_0, morf_0, by = 1000)
axis(1, at = ticks, labels = ticks)
lines((mor1_0:morf_0),X_0_zgo3[(mor1_0:morf_0)],type='l',lwd=2)
points(dMor_0_zgo3[,1],dMor_0_zgo3[,2],pch=19,cex=2,col='blue')

# evolution erreur
plot(c(0:(nm-2)),mean_err,type='b',xlab='Number of missing moraines',ylab='Mean of the MSE ',main='Evolution of the MSE between the real trajectory and the simulations, depending on the number of missing moraines')

