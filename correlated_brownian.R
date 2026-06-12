set.seed(123)

# Paramètres
T  <- 1
n  <- 1000
dt <- T / n
rho <- 0.8

# Incréments browniens indépendants
dB1 <- rnorm(n, mean = 0, sd = sqrt(dt))
dB2 <- rnorm(n, mean = 0, sd = sqrt(dt))

# Construction des browniens corrélés
dW1 <- dB1
dW2 <- rho * dB1 + sqrt(1 - rho^2) * dB2

# Trajectoires
W1 <- c(0, cumsum(dW1))
W2 <- c(0, cumsum(dW2))

# Temps
t <- seq(0, T, length.out = n + 1)

# Affichage
plot(t, W1, type = "l", col = "blue", lwd = 2,
     ylim = range(c(W1, W2)),
     xlab = "Temps", ylab = "Valeur",
     main = "Browniens corrélés")

lines(t, W2, col = "red", lwd = 2)

legend("topleft",
       legend = c("W1", "W2"),
       col = c("blue", "red"),
       lwd = 2)

cor(dW1, dW2)


# multidimensionnel

set.seed(123)

# Paramètres
n_assets <- 4
n_steps  <- 1000
T        <- 1
dt       <- T / n_steps

# Matrice de corrélation
Sigma <- matrix(c(
  1.0, 0.7, 0.3, 0.2,
  0.7, 1.0, 0.5, 0.1,
  0.3, 0.5, 1.0, 0.4,
  0.2, 0.1, 0.4, 1.0
), nrow = 4, byrow = TRUE)

# Décomposition de Cholesky
R <- chol(Sigma)

# Matrice des incréments indépendants
Z <- matrix(
  rnorm(n_assets * n_steps),
  nrow = n_assets
)

# Incréments corrélés
dW <- sqrt(dt) * t(R) %*% Z

# Construction des trajectoires
W <- apply(dW, 1, cumsum)

# Ajouter W_0 = 0
W <- rbind(rep(0, n_assets), W)

# Temps
time <- seq(0, T, length.out = n_steps + 1)

# Plot
matplot(
  time,
  W,
  type = "l",
  lwd = 2,
  xlab = "Temps",
  ylab = "Valeur",
  main = "Browniens corrélés"
)

cor(t(dW))


set.seed(123)

T  <- 1
n  <- 1000
dt <- T/n

rho_values <- c(0, 0.5, 0.9)

par(mfrow = c(3,1))

for(rho in rho_values){
  
  dB1 <- rnorm(n, 0, sqrt(dt))
  dB2 <- rnorm(n, 0, sqrt(dt))
  
  dW1 <- dB1
  dW2 <- rho * dB1 + sqrt(1-rho^2) * dB2
  
  W1 <- c(0, cumsum(dW1))
  W2 <- c(0, cumsum(dW2))
  
  t <- seq(0, T, length.out = n+1)
  
  plot(t, W1, type="l", col="blue", lwd=2,
       ylim=range(c(W1,W2)),
       main=paste("rho =", rho),
       xlab="Temps", ylab="Valeur")
  
  lines(t, W2, col="red", lwd=2)
}

### pont brownien 
par(mfrow = c(1,1))

set.seed(123)

n_assets <- 3
T <- 1
n <- 100
dt <- T/n

Sigma <- matrix(c(
  1, 0.7, 0.3,
  0.7, 1, 0.5,
  0.3, 0.5, 1
), 3, 3)

R <- chol(Sigma)

Z <- matrix(rnorm(n_assets * n), nrow = n_assets)

dW <- sqrt(dt) * t(R) %*% Z
W <- matrix(0, n_assets, n + 1)

for(i in 1:n_assets){
  W[i, 2:(n+1)] <- cumsum(dW[i, ])
}

time <- seq(0, T, length.out = n+1)

a <- c(0, -2, -1)
b <- c(2, -1, 3)

X <- matrix(0, n_assets, n+1)

for(i in 1:n_assets){
  X[i, ] <- a[i] + (time/T)*(b[i] - a[i]) +
    (W[i, ] - (time/T) * W[i, n+1])
}


matplot(time, t(X), type="l", lwd=2,
        main="Pont brownien corrélé multidimensionnel",
        xlab="t", ylab="X_t")


### avec plusieurs records

x_r<-c(0,50,120,500)
y_r<-c(0,10,45,50)
n_r<-x_r[-1]-x_r[-4]

N<-length(x_r)-1


n <- 4
alpha <- 0.3

Sigma <- matrix(0, n, n)

for(i in 1:n){
  for(j in 1:n){
    Sigma[i,j] <- exp(-alpha * abs(i-j))
  }
}

results<-matrix(0,ncol=x_r[-(length(x_r))],nrow=n)
for(i in N){
  
}

