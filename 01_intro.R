### toymodel
## chooseCRANmirror(graphics = FALSE)

Time <- 1000
Ni <- 100

gamma <- 1.1 # fraction of reinvested profit
phi <- 0.1 # capital productivity
r <- 0.1 # interest rate
Pbar <- 0.01 # random price drift

A <- matrix(data = 1, ncol = 1, nrow = Ni) # net worth
K <- matrix(data = 1, ncol = 1, nrow = Ni) # capital
B <- matrix(data = 0, ncol = 1, nrow = Ni) # debt
I <- matrix(data = 0, ncol = 1, nrow = Ni) # investment
P <- matrix(data = 0, ncol = 1, nrow = Ni) # price
Y <- matrix(data = 0, ncol = 1, nrow = Ni) # production
Z <- matrix(2 * runif (Ni) + Pbar , ncol = 1, nrow = Ni) # profit
YY <- matrix(data = 0, ncol = 1, nrow = Time) # aggregate prod

## main program
for(t in 2:Time) {
    I <- gamma * Z # investment choice
    K <- K + I # capital accumulation
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
}

plot(2:Time,
     YY[2:Time, 1],
     type = 'l',
     ylim = range(YY[2:Time]),
     col = 1,
     ylab = 'YY',
     xlab = 't')
