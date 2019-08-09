### toymodel
## chooseCRANmirror(graphics = FALSE)

set.seed(15)

Time <- 1000
Ni <- 100

gamma <- 2 ## fraction of reinvested profit
phi <- 0.1 ## capital productivity
r <- 0.1 ## interest rate
Pbar <- 0.01 ## random price drift
delta <- 0.05 ## depreciacion rate

A <- matrix(data = 1, ncol = 1, nrow = Ni) ## net worth
K <- matrix(data = 1, ncol = 1, nrow = Ni) ## capital
B <- matrix(data = 0, ncol = 1, nrow = Ni) ## debt
I <- matrix(data = 0, ncol = 1, nrow = Ni) ## investment
P <- matrix(data = 0, ncol = 1, nrow = Ni) ## price
Y <- matrix(data = 0, ncol = 1, nrow = Ni) ## production
Z <- matrix(2 * runif (Ni) + Pbar , ncol = 1, nrow = Ni) ## profit
YY <- matrix(data = 0, ncol = 1, nrow = Time) ## aggregate prod
AA <- matrix(data = 0, ncol = 1, nrow = Time) ## aggregate prod
BB <- matrix(data = 0, ncol = 1, nrow = Time) ## aggregate prod

## main program
for(t in 2:Time) {
    I <- gamma * Z # investment choice
    I[Z<0] <- 0
    K <- K * (1 - delta) + I # capital accumulation
    Y <- phi * K # production
    B <- K - A # debt
    B[B<0] <- 0 # self-financed firms
    P <- 2 * runif(Ni) + Pbar # stochastic price
    Z <- P * Y - r * K # profit
    A <- A + Z # net worth
    Z[A<0] <- 0 # entry condition
    K[A<0] <- 1 # entry condition
    A[A<0] <- 1 # entry-exit process
    YY[t] <- sum(Y) # aggr prod
    AA[t] <- sum(A) # net worth
    BB[t] <- sum(B)
}

## aggregate production
plot(2:Time,
     log(YY[2:Time, 1]),
     type = 'l',
     ylim = range(log(YY[2:Time])),
     col = 1,
     ylab = 'log(YY)',
     xlab = 't')

## net worth
plot(2:Time,
     log(AA[2:Time, 1]),
     type = 'l',
     ylim = range(log(AA[2:Time])),
     col = 1,
     ylab = 'log(AA)',
     xlab = 't')

## aggregate debt
plot(2:Time,
     log(BB[2:Time, 1]),
     type = 'l',
     ylim = range(log(BB[2:Time])),
     col = 1,
     ylab = 'log(BB)',
     xlab = 't')
