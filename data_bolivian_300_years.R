library(readxl)
library(simrec)
library(scales)
#-------------------------------------------------------------------------------
# Données des glaciers
#-------------------------------------------------------------------------------

CN_colder <- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/Glaciers_warming_trend/CN_colder_condition.dat", sep = "", header = FALSE)
CN_warmer <- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/Glaciers_warming_trend/CN_warmer_condition.dat", sep = "", header = FALSE)

CS_colder <- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/Glaciers_warming_trend/CS_colder_condition.dat", sep = "", header = FALSE)
CS_warmer <- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/Glaciers_warming_trend/CS_warmer_condition.dat", sep = "", header = FALSE)

T_colder <- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/Glaciers_warming_trend/Telata_colder_condition.dat", sep = "", header = FALSE)
T_warmer <- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/Glaciers_warming_trend/Telata_warmer_condition.dat", sep = "", header = FALSE)

Z_colder <- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/Glaciers_warming_trend/Zongo_colder_condition.dat", sep = "", header = FALSE)
Z_warmer <- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/Glaciers_warming_trend/Zongo_warmer_condition.dat", sep = "", header = FALSE)

# Altitudes 

CN_C_z<-CN_colder$V3[-1]
CN_W_z<-CN_warmer$V3[-1]

CS_C_z<-CS_colder$V3[-1]
CS_W_z<-CS_warmer$V3[-1]

T_C_z<-T_colder$V3[-1]
T_W_z<-T_warmer$V3[-1]

Z_C_z<-Z_colder$V3[-1]
Z_W_z<-Z_warmer$V3[-1]

par(mfrow=c(2,2))

plot(CN_C_z,type='l',col='blue',ylim=c(4400,5000),ylab='Altitude',xlab='Time (years)')
lines(CN_W_z,type='l',col='red')
legend('bottomright', legend=c('Cold','Warm'),col=c('blue','red'),lwd=1,bty='n')
legend('topleft',legend=c('Charquini North'),bty='n')

plot(CS_C_z,type='l',col='blue',ylim=c(4400,5000),ylab='Altitude',xlab='Time (years)')
lines(CS_W_z,type='l',col='red')
legend('topleft',legend=c('Charquini South'),bty='n')

plot(T_C_z,type='l',col='blue',ylim=c(4400,5000),ylab='Altitude',xlab='Time (years)')
lines(T_W_z,type='l',col='red')
legend('topleft',legend=c('Telata'),bty='n')

plot(Z_C_z,type='l',col='blue',ylim=c(4400,5000),ylab='Altitude',xlab='Time (years)')
lines(Z_W_z,type='l',col='red')
legend('topleft',legend=c('Zongo'),bty='n')

# Volumes
CN_C_v<-CN_colder$V4[-1]
CN_W_v<-CN_warmer$V4[-1]

CS_C_v<-CS_colder$V4[-1]
CS_W_v<-CS_warmer$V4[-1]

T_C_v<-T_colder$V4[-1]
T_W_v<-T_warmer$V4[-1]

Z_C_v<-Z_colder$V4[-1]
Z_W_v<-Z_warmer$V4[-1]

# areas
CN_C_a<-CN_colder$V5[-1]
CN_W_a<-CN_warmer$V5[-1]

CS_C_a<-CS_colder$V5[-1]
CS_W_a<-CS_warmer$V5[-1]

T_C_a<-T_colder$V5[-1]
T_W_a<-T_warmer$V5[-1]

Z_C_a<-Z_colder$V5[-1]
Z_W_a<-Z_warmer$V5[-1]

# mean glacier thickness
CN_C_t<-CN_colder$V6[-1]
CN_W_t<-CN_warmer$V6[-1]

CS_C_t<-CS_colder$V6[-1]
CS_W_t<-CS_warmer$V6[-1]

T_C_t<-T_colder$V6[-1]
T_W_t<-T_warmer$V6[-1]

Z_C_t<-Z_colder$V6[-1]
Z_W_t<-Z_warmer$V6[-1]



# plot normalisés
par(mfrow=c(2,2))
# warm
plot(scale(as.integer(CN_W_z)),type='l',col='red',xlab='Time (years)',ylab='Scaled variations',ylim=c(-4,3))
lines(scale(as.integer(CN_W_v)),type='l',col='black')
lines(scale(as.integer(CN_W_a)),type='l',col='green')
lines(scale(as.integer(CN_W_t)),type='l',col='orange')
abline(h=0,lty=2,col='grey')
legend('bottom', legend=c('Altitude','Volume','Area','Thikness'),col=c('red','black','green','orange'),lwd=1,bty='n')
legend('topleft',legend=c('Charquini North'),bty='n')

plot(scale(as.integer(CS_W_z)),type='l',col='red',xlab='Time (years)',ylab='Scaled variations',ylim=c(-4,3))
lines(scale(as.integer(CS_W_v)),type='l',col='black')
lines(scale(as.integer(CS_W_a)),type='l',col='green')
lines(scale(as.integer(CS_W_t)),type='l',col='orange')
abline(h=0,lty=2,col='grey')
legend('topleft',legend=c('Charquini South'),bty='n')

plot(scale(as.integer(T_W_z)),type='l',col='red',xlab='Time (years)',ylab='Scaled variations',ylim=c(-4,3))
lines(scale(as.integer(T_W_v)),type='l',col='black')
lines(scale(as.integer(T_W_a)),type='l',col='green')
lines(scale(as.integer(T_W_t)),type='l',col='orange')
abline(h=0,lty=2,col='grey')
legend('topleft',legend=c('Telata'),bty='n')

plot(scale(as.integer(Z_W_z)),type='l',col='red',xlab='Time (years)',ylab='Scaled variations',ylim=c(-4,3))
lines(scale(as.integer(Z_W_v)),type='l',col='black')
lines(scale(as.integer(Z_W_a)),type='l',col='green')
lines(scale(as.integer(Z_W_t)),type='l',col='orange')
abline(h=0,lty=2,col='grey')
legend('topleft',legend=c('Zongo'),bty='n')


# cold
plot(scale(as.integer(CN_C_z)),type='l',col='blue',xlab='Time (years)',ylab='Scaled variations',ylim=c(-4,3))
lines(scale(as.integer(CN_C_v)),type='l',col='black')
lines(scale(as.integer(CN_C_a)),type='l',col='green')
lines(scale(as.integer(CN_C_t)),type='l',col='orange')
abline(h=0,lty=2,col='grey')
legend('bottom', legend=c('Altitude','Volume','Area','Thikness'),col=c('blue','black','green','orange'),lwd=1,bty='n')
legend('topleft',legend=c('Charquini North'),bty='n')

plot(scale(as.integer(CS_C_z)),type='l',col='blue',xlab='Time (years)',ylab='Scaled variations',ylim=c(-4,3))
lines(scale(as.integer(CS_C_v)),type='l',col='black')
lines(scale(as.integer(CS_C_a)),type='l',col='green')
lines(scale(as.integer(CS_C_t)),type='l',col='orange')
abline(h=0,lty=2,col='grey')
legend('topleft',legend=c('Charquini South'),bty='n')

plot(scale(as.integer(T_C_z)),type='l',col='blue',xlab='Time (years)',ylab='Scaled variations',ylim=c(-4,3))
lines(scale(as.integer(T_C_v)),type='l',col='black')
lines(scale(as.integer(T_C_a)),type='l',col='green')
lines(scale(as.integer(T_C_t)),type='l',col='orange')
abline(h=0,lty=2,col='grey')
legend('topleft',legend=c('Telata'),bty='n')

plot(scale(as.integer(Z_C_z)),type='l',col='blue',xlab='Time (years)',ylab='Scaled variations',ylim=c(-4,3))
lines(scale(as.integer(Z_C_v)),type='l',col='black')
lines(scale(as.integer(Z_C_a)),type='l',col='green')
lines(scale(as.integer(Z_C_t)),type='l',col='orange')
abline(h=0,lty=2,col='grey')
legend('topleft',legend=c('Zongo'),bty='n')

col4g<-c('#2F09C8', '#079C25', '#B31E50', '#B37F1E')

# plot plusieurs glaciers

par(mfrow=c(2,1))
plot(CN_C_z,type='l',col=col4g[1],ylim=c(4400,5000),xlab='Time (years)',ylab='Altitude variations',lwd=2)
lines(CS_C_z,type='l',col=col4g[2],lwd=2)
lines(T_C_z,type='l',col=col4g[3],lwd=2)
lines(Z_C_z,type='l',col=col4g[4],lwd=2)
legend('topleft',legend=c('Cold climate'),bty='n')

plot(CN_W_z,type='l',col=col4g[1],ylim=c(4400,5000),xlab='Time (years)',ylab='Altitude variations',lwd=2)
lines(CS_W_z,type='l',col=col4g[2],lwd=2)
lines(T_W_z,type='l',col=col4g[3],lwd=2)
lines(Z_W_z,type='l',col=col4g[4],lwd=2)
legend('bottomleft',legend=c('Warm climate'),bty='n')
legend('bottomright', legend=c('Charquini N','Charquini S','Telata','Zongo'),col=col4g,lwd=2,bty='n')


# plot plusieurs glaciers scaled

par(mfrow=c(2,1))
plot(scale(as.integer(CN_C_z)),type='l',col=col4g[1],xlab='Time (years)',ylab='Scaled altitude variations',ylim=c(-3,3),lwd=2)
lines(scale(as.integer(CS_C_z)),type='l',col=col4g[2],lwd=2)
lines(scale(as.integer(T_C_z)),type='l',col=col4g[3],lwd=2)
lines(scale(as.integer(Z_C_z)),type='l',col=col4g[4],lwd=2)
abline(h=0,lty=2,col='grey')
legend('topleft',legend=c('Cold climate'),bty='n')

plot(scale(as.integer(CN_W_z)),type='l',xlab='Time (years)',ylab='Scaled altitude variations',ylim=c(-3,3),col=col4g[1],lwd=2)
lines(scale(as.integer(CS_W_z)),type='l',col=col4g[2],lwd=2)
lines(scale(as.integer(T_W_z)),type='l',col=col4g[3],lwd=2)
lines(scale(as.integer(Z_W_z)),type='l',,col=col4g[4],lwd=2)
abline(h=0,lty=2,col='grey')
legend('topleft',legend=c('Warm climate'),bty='n')
legend('bottomright', legend=c('Charquini N','Charquini S','Telata','Zongo'),col=col4g,lwd=2,bty='n')

#-------------------------------------------------------------------------------
# automatisation de la détection des moraines
#-------------------------------------------------------------------------------
detect_records_maxima_norm<-function(X){
  l=length(X)
  rec_max=c(X[1])
  ind_max<-c(1)
  
  for(i in 2:(l-1)){
    #print(i)
    if(X[i-1]<=X[i] && X[i]>X[i+1]){
      l<-length(rec_max)
      if(X[i]> rec_max[l]){
        rec_max<-c(rec_max,X[i])
        ind_max<-c(ind_max,i)
      }
    }
  }
  df<-data.frame(rec_max,ind_max)
  return(df)
}

moraines<-function(series,traj=TRUE){
  year_NA<-1:(length(series))
  pos_inv_NA<-rev(series)
  X_NA<--as.numeric(pos_inv_NA)
  X_0<-X_NA
  X_0[which(is.na(X_0))]<-0
  l_1_0<-(detect_records_maxima_norm(X_0)$ind_max[-1])[1]
  l_ind_0<-detect_records_maxima_norm(X_0)$ind_max[-1]
  recordTimes_0=l_ind_0
  recordValues_0=(detect_records_maxima_norm(X_0)$rec_max[-1])
  dMor_0=cbind(recordTimes_0,recordValues_0)
  mor1_0=dMor_0[1,1]
  morf_0=dMor_0[length(dMor_0[,1]),1]
  years_mor<-(mor1_0:morf_0)[dMor_0[,1]]
  pos_0_NA<-pos_inv_NA
  pos_0_NA[which(is.na(pos_0_NA))]<-0
  if(traj){
    return(data.frame(time=(mor1_0:morf_0),pos=X_NA[mor1_0:morf_0]))
  }
  else{
    return(dMor_0)
  }
}

# exemple 
par(mfrow=c(1,1))
plot(moraines(CN_C_z,traj=TRUE),type='l',col='red')
points(moraines(CN_C_z,traj=FALSE),pch=19,cex=1,col='red')
legend('bottomright',legend=c('Moraines','Collected data'),pch=c(19,NA),lwd=c(NA,2),col='red')

#-------------------------------------------------------------------------------
# Simulations browniennes
#-------------------------------------------------------------------------------

# CN
## cold
dMor_CN_C<-moraines(CN_C_z,traj=FALSE)
dMor_CN_C
traj_CN_C<-moraines(CN_C_z,traj=TRUE)
## warm
dMor_CN_W<-moraines(CN_W_z,traj=FALSE)
traj_CN_W<-moraines(CN_W_z,traj=TRUE)
dMor_CN_W
# CS
## cold
dMor_CS_C<-moraines(CS_C_z,traj=FALSE)
traj_CS_C<-moraines(CS_C_z,traj=TRUE)
dMor_CS_C
## warm
dMor_CS_W<-moraines(CS_W_z,traj=FALSE)
traj_CS_W<-moraines(CS_W_z,traj=TRUE)
dMor_CS_W
#T
## cold
dMor_T_C<-moraines(T_C_z,traj=FALSE)
traj_T_C<-moraines(T_C_z,traj=TRUE)
dMor_T_C
## warmp
dMor_T_W<-moraines(T_W_z,traj=FALSE)
traj_T_W<-moraines(T_W_z,traj=TRUE)
dMor_T_W

# Z
## cold
dMor_Z_C<-moraines(Z_C_z,traj=FALSE)
traj_Z_C<-moraines(Z_C_z,traj=TRUE)
dMor_Z_C
## warm
dMor_Z_W<-moraines(Z_W_z,traj=FALSE)
traj_Z_W<-moraines(Z_W_z,traj=TRUE)
dMor_Z_W

### nombre moyen de moraines en fct du temps

t<-1:300

# CN C
is_mor_CN_C<-rep(0,300)
for(i in (1:length(dMor_CN_C[,1]))){
  is_mor_CN_C[dMor_CN_C[i,1]]<-1
}
S_CN_C<-cumsum(is_mor_CN_C)
plot(S_CN_C)

# CN W
is_mor_CN_W<-rep(0,300)
for(i in (1:length(dMor_CN_W[,1]))){
  is_mor_CN_W[dMor_CN_W[i,1]]<-1
}
S_CN_W<-cumsum(is_mor_CN_W)
plot(S_CN_W)

#CS C
is_mor_CS_C<-rep(0,300)
for(i in (1:length(dMor_CS_C[,1]))){
  is_mor_CS_C[dMor_CS_C[i,1]]<-1
}
S_CS_C<-cumsum(is_mor_CS_C)
plot(S_CS_C)

# CS W
is_mor_CS_W<-rep(0,300)
for(i in (1:length(dMor_CS_W[,1]))){
  is_mor_CS_W[dMor_CS_W[i,1]]<-1
}
S_CS_W<-cumsum(is_mor_CS_W)
plot(S_CS_W)

#T C
is_mor_T_C<-rep(0,300)
for(i in (1:length(dMor_T_C[,1]))){
  is_mor_T_C[dMor_T_C[i,1]]<-1
}
S_T_C<-cumsum(is_mor_T_C)
plot(S_T_C)

# T W
is_mor_T_W<-rep(0,300)
for(i in (1:length(dMor_T_W[,1]))){
  is_mor_T_W[dMor_T_W[i,1]]<-1
}
S_T_W<-cumsum(is_mor_T_W)
plot(S_T_W)

#Z C
is_mor_Z_C<-rep(0,300)
for(i in (1:length(dMor_Z_C[,1]))){
  is_mor_Z_C[dMor_Z_C[i,1]]<-1
}
S_Z_C<-cumsum(is_mor_Z_C)
plot(S_Z_C)

# Z W
is_mor_Z_W<-rep(0,300)
for(i in (1:length(dMor_Z_W[,1]))){
  is_mor_Z_W[dMor_Z_W[i,1]]<-1
}
S_Z_W<-cumsum(is_mor_Z_W)
plot(S_Z_W)

sums<-data.frame(S_CN_C,S_CN_W,S_CS_C,S_CS_W,S_T_C,S_T_W,S_Z_C,S_Z_W)
mean_sums<-rowMeans(sums)
plot(mean_sums,pch=19,col='black',type='b',cex=0.5,xlab='Time (years)',ylab='Number of moraines',main='Mean of the number of moraines for the 4 glaciers in both climates')

sums_C<-data.frame(S_CN_C,S_CS_C,S_T_C,S_Z_C)
mean_sums_C<-rowMeans(sums_C)
plot(mean_sums_C,pch=19,col='blue',type='b',cex=0.5,xlab='Time (years)',ylab='Number of moraines',main='Mean of the number of moraines for the 4 glaciers in cold climate')

sums_W<-data.frame(S_CN_W,S_CS_W,S_T_W,S_Z_W)
mean_sums_W<-rowMeans(sums_W)
plot(mean_sums_W,pch=19,col='red',type='b',cex=0.5,xlab='Time (years)',ylab='Number of moraines',main='Mean of the number of moraines for the 4 glaciers in warm climate')

plot(mean_sums,pch=19,col='black',type='b',cex=0.5,xlab='Time (years)',ylab='Number of moraines',main='Mean of the number of moraines for the 4 glaciers in both climates',ylim=c(0,10))
lines(mean_sums_C,pch=19,col='blue',type='b',cex=0.5)
lines(mean_sums_W,pch=19,col='red',type='b',cex=0.5)     
legend('bottomright',legend=c('Cold climate','Warm climate','Both'),col=c('blue','red','black'),pch=19,bty='n')

par(mfrow=(c(1,2)))
plot(S_CN_C,pch=19,col='blue',type='l',cex=0.5,lwd=2,xlab='Time (years)',ylab='Number of moraines',main='Moraines of the 4 glaciers in cold climate')
lines(S_CS_C,pch=19,col='blue2',type='l',cex=0.5,lwd=2)
lines(S_T_C,pch=19,col='blue3',type='l',cex=0.5,lwd=2)
lines(S_Z_C,pch=19,col='blue4',type='l',cex=0.5,lwd=2)


plot(S_CN_W,pch=19,col='red',type='l',cex=0.5,lwd=2,xlab='Time (years)',ylab='Number of moraines',main='Moraines of the 4 glaciers in warm climate')
lines(S_CS_W,pch=19,col='red2',type='l',cex=0.5,lwd=2)
lines(S_T_W,pch=19,col='red3',type='l',cex=0.5,lwd=2)
lines(S_Z_W,pch=19,col='red4',type='l',cex=0.5,lwd=2)

# plot

par(mfrow=(c(2,2)))
plot(traj_CN_C,col='blue',type='l',ylim=c(-5000,-4400),xlim=c(0,300),xlab='Reversed time (years)',ylab='Reversed altitude (m)')
lines(traj_CN_W,col='red',type='l')
points(dMor_CN_C,pch=19,col='blue')
points(dMor_CN_W,pch=19,col='red')
legend('topleft',legend=c('Charquini North'),bty='n')

plot(traj_CS_C,col='blue',type='l',ylim=c(-5000,-4400),xlim=c(0,300),xlab='Reversed time (years)',ylab='Reversed altitude (m)')
lines(traj_CS_W,col='red',type='l')
points(dMor_CS_C,pch=19,col='blue')
points(dMor_CS_W,pch=19,col='red')
legend('topleft',legend=c('Charquini South'),bty='n')

plot(traj_T_C,col='blue',type='l',ylim=c(-5000,-4400),xlim=c(0,300),xlab='Reversed time (years)',ylab='Reversed altitude (m)')
lines(traj_T_W,col='red',type='l')
points(dMor_T_C,pch=19,col='blue')
points(dMor_T_W,pch=19,col='red')
legend('topleft',legend=c('Telata'),bty='n')

plot(traj_Z_C,col='blue',type='l',ylim=c(-5000,-4400),xlim=c(0,300),xlab='Reversed time (years)',ylab='Reversed altitude (m)')
lines(traj_Z_W,col='red',type='l')
points(dMor_Z_C,pch=19,col='blue')
points(dMor_Z_W,pch=19,col='red')
legend('topleft',legend=c('Zongo'),bty='n')
legend('bottomright', legend=c('Cold','Warm','Moraines'),col=c('blue','red','black'),lwd=c(1,1,NA),pch=c(NA,NA,19),bty='n')

par(mfrow=c(1,2))

plot(traj_CN_C,col=col4g[1],type='l',ylim=c(-5000,-4400),xlim=c(0,300),xlab='Reversed time (years)',ylab='Reversed altitude (m)')
lines(traj_CS_C,col=col4g[2],type='l')
lines(traj_T_C,col=col4g[3],type='l')
lines(traj_Z_C,col=col4g[4],type='l')
points(dMor_CN_C,pch=19,col=col4g[1])
points(dMor_CS_C,pch=19,col=col4g[2])
points(dMor_T_C,pch=19,col=col4g[3])
points(dMor_Z_C,pch=19,col=col4g[4])
segments(x0 = dMor_CN_C[,1],
         y0 = -5100,
         x1 = dMor_CN_C[,1],
         y1 = dMor_CN_C[,2],
         col = col4g[1],
         lty = 2)
segments(x0 = dMor_CS_C[,1],
         y0 = -5100,
         x1 = dMor_CS_C[,1],
         y1 = dMor_CS_C[,2],
         col = col4g[2],
         lty = 2)
segments(x0 = dMor_T_C[,1],
         y0 = -5100,
         x1 = dMor_T_C[,1],
         y1 = dMor_T_C[,2],
         col = col4g[3],
         lty = 2)
segments(x0 = dMor_Z_C[,1],
         y0 = -5100,
         x1 = dMor_Z_C[,1],
         y1 = dMor_Z_C[,2],
         col = col4g[4],
         lty = 2)
legend('topleft',legend=c('Cold climate'),bty='n')

plot(traj_CN_W,col=col4g[1],type='l',ylim=c(-5000,-4400),xlim=c(0,300),xlab='Reversed time (years)',ylab='Reversed altitude (m)')
lines(traj_CS_W,col=col4g[2],type='l')
lines(traj_T_W,col=col4g[3],type='l')
lines(traj_Z_W,col=col4g[4],type='l')
points(dMor_CN_W,pch=19,col=col4g[1])
points(dMor_CS_W,pch=19,col=col4g[2])
points(dMor_T_W,pch=19,col=col4g[3])
points(dMor_Z_W,pch=19,col=col4g[4])
segments(x0 = dMor_CN_W[,1],
         y0 = -5100,
         x1 = dMor_CN_W[,1],
         y1 = dMor_CN_W[,2],
         col = col4g[1],
         lty = 2)
segments(x0 = dMor_CS_W[,1],
         y0 = -5100,
         x1 = dMor_CS_W[,1],
         y1 = dMor_CS_W[,2],
         col = col4g[2],
         lty = 2)
segments(x0 = dMor_T_W[,1],
         y0 = -5100,
         x1 = dMor_T_W[,1],
         y1 = dMor_T_W[,2],
         col = col4g[3],
         lty = 2)
segments(x0 = dMor_Z_W[,1],
         y0 = -5100,
         x1 = dMor_Z_W[,1],
         y1 = dMor_Z_W[,2],
         col = col4g[4],
         lty = 2)
legend('topleft',legend=c('Warm climate'),bty='n')
legend('topright', legend=c('Charquini N','Charquini S','Telata','Zongo'),col=col4g,lwd=2,bty='n')


par(mfrow=(c(2,2)))
N<-300

# warm

SigmaS=0.72
Sd=2.19
traj_sim <- simRecords(dMor_CN_W, N=N,gaussConv = TRUE,sigmaGauss = SigmaS,sd = Sd)
temps<-traj_sim[,1]
position<-traj_sim[,2]
plot(temps,position,col='black',type='l',ylim=c(-5000,-4850))
lines(traj_CN_W,col='red',type='l')
points(dMor_CN_W,pch=19,col='red')
legend('topleft',legend=c('Charquini North'),bty='n')
legend('bottomright',legend=c('Elmer trajectory','Brownian simulation'),col=c('red','black'),lty=1,bty='n')


SigmaS=0.12
Sd=2.64
traj_sim <- simRecords(dMor_CS_W, N=N,gaussConv = TRUE,sigmaGauss = SigmaS,sd = Sd)
temps<-traj_sim[,1]
position<-traj_sim[,2]
plot(temps,position,col='black',type='l',ylim=c(-4850,-4750))
lines(traj_CS_W,col='red',type='l')
points(dMor_CS_W,pch=19,col='red')
legend('topleft',legend=c('Charquini South'),bty='n')

SigmaS=0.4
Sd=6.5
traj_sim <- simRecords(dMor_T_W, N=N,gaussConv = TRUE,sigmaGauss = SigmaS,sd = Sd)
temps<-traj_sim[,1]
position<-traj_sim[,2]
plot(temps,position,col='black',type='l',ylim=c(-4900,-4750))
lines(traj_T_W,col='red',type='l')
points(dMor_T_W,pch=19,col='red')
legend('topleft',legend=c('Telata'),bty='n')

SigmaS=0.66
Sd=3.05
traj_sim <- simRecords(dMor_Z_W, N=N,gaussConv = TRUE,sigmaGauss = SigmaS,sd = Sd)
temps<-traj_sim[,1]
position<-traj_sim[,2]
plot(temps,position,col='black',type='l',ylim=c(-4850,-4700))
lines(traj_Z_W,col='red',type='l')
points(dMor_Z_W,pch=19,col='red')
legend('topleft',legend=c('Zongo'),bty='n')

# cold

par(mfrow=(c(2,2)))

SigmaS=1.75
Sd=1.5
traj_sim <- simRecords(dMor_CN_C, N=N,gaussConv = TRUE,sigmaGauss = SigmaS,sd = Sd)
temps<-traj_sim[,1]
position<-traj_sim[,2]
plot(temps,position,col='black',type='l')
lines(traj_CN_C,col='blue',type='l')
points(dMor_CN_C,pch=19,col='blue')
legend('topleft',legend=c('Charquini North'),bty='n')
legend('bottomright',legend=c('Elmer trajectory','Brownian simulation'),col=c('blue','black'),lty=1,bty='n')


SigmaS=0.21
Sd=2.3
traj_sim <- simRecords(dMor_CS_C, N=N,gaussConv = TRUE,sigmaGauss = SigmaS,sd = Sd)
temps<-traj_sim[,1]
position<-traj_sim[,2]
plot(temps,position,col='black',type='l')
lines(traj_CS_C,col='blue',type='l')
points(dMor_CS_C,pch=19,col='blue')
legend('topleft',legend=c('Charquini South'),bty='n')

SigmaS=0.34
Sd=3.25
traj_sim <- simRecords(dMor_T_C, N=N,gaussConv = TRUE,sigmaGauss = SigmaS,sd = Sd)
temps<-traj_sim[,1]
position<-traj_sim[,2]
plot(temps,position,col='black',type='l')
lines(traj_T_C,col='blue',type='l')
points(dMor_T_C,pch=19,col='blue')
legend('topleft',legend=c('Telata'),bty='n')

SigmaS=0.3
Sd=2.83
traj_sim <- simRecords(dMor_Z_C, N=N,gaussConv = TRUE,sigmaGauss = SigmaS,sd = Sd)
temps<-traj_sim[,1]
position<-traj_sim[,2]
plot(temps,position,col='black',type='l')
lines(traj_Z_C,col='blue',type='l')
points(dMor_Z_C,pch=19,col='blue')
legend('topleft',legend=c('Zongo'),bty='n')




#-------------------------------------------------------------------------------
# Données du climat
#-------------------------------------------------------------------------------

P <- read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/Glaciers_warming_trend/Precip_300y.dat", sep = "", header = FALSE)$V1
T_C<-read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/Glaciers_warming_trend/Temp_300y_cold.dat", sep = "", header = FALSE)$V1
T_W<-read.delim("/home/mmegret/Documents/thèse Maud MEGRET/données/bolivian/Glaciers_warming_trend/Temp_300y_warm.dat", sep = "", header = FALSE)$V1

par(mfrow=c(1,1))

years<-300

moy_year_P<-matrix(ncol=1,nrow=years)
for(i in 1:years ){
  moy_year_P[i,]<-mean(as.numeric(P[(365*(i-1)+1):(365*i)]),na.rm=TRUE)
}

moy_year_TC<-matrix(ncol=1,nrow=years)
for(i in 1:years ){
  moy_year_TC[i,]<-mean(as.numeric(T_C[(365*(i-1)+1):(365*i)]),na.rm=TRUE)
}
moy_year_TW<-matrix(ncol=1,nrow=years)
for(i in 1:years ){
  moy_year_TW[i,]<-mean(as.numeric(T_W[(365*(i-1)+1):(365*i)]),na.rm=TRUE)
}

# plots
par(mfrow=c(1,1))
plot(moy_year_P,type='l',col='grey',xlab='Time (years)',ylab='Precipitations')
plot(moy_year_TC,type='l',col='blue')
plot(moy_year_TW,type='l',col='red')

plot(moy_year_TC,type='l',col='blue',ylim=c(-3,1),xlab='Time (years)',ylab='Climate variations (°C)')
lines(moy_year_TW,type='l',col='red')
legend('bottomright',legend=c('Cold','Warm'),col=c('blue','red'),lwd=1,bty='n')

par(mfrow=c(1,2))
plot(scale(moy_year_TC),type='l',col=alpha('purple',0.5),xlab='Time (years)',ylab='Scaled variations',ylim=c(-4.5,3))
lines(scale(moy_year_P),col='grey')
lines(scale(as.numeric(CN_C_z)),col=col4g[1],type='l',lwd=2)
lines(scale(as.numeric(CS_C_z)),col=col4g[2],type='l',lwd=2)
lines(scale(as.numeric(T_C_z)),col=col4g[3],type='l',lwd=2)
lines(scale(as.numeric(Z_C_z)),col=col4g[4],type='l',lwd=2)
legend('topleft',legend='Cold climate',bty='n')
legend('bottomright', legend=c('Temperature','Precipitations'),col=c(alpha('purple',0.5),'grey'),lwd=2,bty='n')

plot(scale(moy_year_TW),type='l',col=alpha('purple',0.5),xlab='Time (years)',ylab='Scaled variations',ylim=c(-4.5,3))
lines(scale(moy_year_P),col='grey')
lines(scale(as.numeric(CN_W_z)),col=col4g[1],type='l',lwd=2)
lines(scale(as.numeric(CS_W_z)),col=col4g[2],type='l',lwd=2)
lines(scale(as.numeric(T_W_z)),col=col4g[3],type='l',lwd=2)
lines(scale(as.numeric(Z_W_z)),col=col4g[4],type='l',lwd=2)
legend('topleft',legend='Warm climate',bty='n')
legend('bottomright', legend=c('Charquini N','Charquini S','Telata','Zongo'),col=col4g,lwd=2,bty='n')

# écart moyen entre deux moraines
# écart en temps
mt_CN_C<-mean((dMor_CN_C[,1])[-1]-(dMor_CN_C[,1])[-length(dMor_CN_C[,1])])
mt_CS_C<-mean((dMor_CS_C[,1])[-1]-(dMor_CS_C[,1])[-length(dMor_CS_C[,1])])
mt_T_C<-mean((dMor_T_C[,1])[-1]-(dMor_T_C[,1])[-length(dMor_T_C[,1])])
mt_Z_C<-mean((dMor_Z_C[,1])[-1]-(dMor_Z_C[,1])[-length(dMor_Z_C[,1])])

mt_CN_W<-mean((dMor_CN_W[,1])[-1]-(dMor_CN_W[,1])[-length(dMor_CN_W[,1])])
mt_CS_W<-mean((dMor_CS_W[,1])[-1]-(dMor_CS_W[,1])[-length(dMor_CS_W[,1])])
mt_T_W<-mean((dMor_T_W[,1])[-1]-(dMor_T_W[,1])[-length(dMor_T_W[,1])])
mt_Z_W<-mean((dMor_Z_W[,1])[-1]-(dMor_Z_W[,1])[-length(dMor_Z_W[,1])])

# écart en position
mp_CN_C<-mean((dMor_CN_C[,2])[-1]-(dMor_CN_C[,2])[-length(dMor_CN_C[,2])])
mp_CS_C<-mean((dMor_CS_C[,2])[-1]-(dMor_CS_C[,2])[-length(dMor_CS_C[,2])])
mp_T_C<-mean((dMor_T_C[,2])[-1]-(dMor_T_C[,2])[-length(dMor_T_C[,2])])
mp_Z_C<-mean((dMor_Z_C[,2])[-1]-(dMor_Z_C[,2])[-length(dMor_Z_C[,2])])

mp_CN_W<-mean((dMor_CN_W[,2])[-1]-(dMor_CN_W[,2])[-length(dMor_CN_W[,2])])
mp_CS_W<-mean((dMor_CS_W[,2])[-1]-(dMor_CS_W[,2])[-length(dMor_CS_W[,2])])
mp_T_W<-mean((dMor_T_W[,2])[-1]-(dMor_T_W[,2])[-length(dMor_T_W[,2])])
mp_Z_W<-mean((dMor_Z_W[,2])[-1]-(dMor_Z_W[,2])[-length(dMor_Z_W[,2])])




