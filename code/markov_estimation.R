##--------------------
## Can we estimate best fitting
## transition matrices from proportions in the states?
## CM, PD: 20/03/2019
##--------------------

## true transition matrix
M <- 4
log_odds <- cbind(0, matrix(rnorm((M-1) * M), nrow = M, ncol = M-1))
p_true <- exp(log_odds) / rowSums(exp(log_odds))

## observed effort
T <- 10 ## time steps
E <- matrix(NA, nrow = M, ncol = T)

E[, 1] <- rep(100, M)

## transition
for(t in 2:T){
    E[, t] <- E[, t-1] %*% p_true
}

matplot(t(E), type = "l", ylim = c(0, max(E)))

## can we estimate p_true from E?

## start out with sum of squared differences between observed and predicted effort
## think about the distribution here - not numbers but effort so not multinomial
## maybe Dirichlet

ssq <- function(theta){
    log_odds <- cbind(0, matrix(theta, nrow = M, ncol = M-1))
    p_est <- exp(log_odds) / rowSums(exp(log_odds))
    E_est <- matrix(NA, nrow = M, ncol = T)
    E_est[, 1] <- E[, 1]
    ## transition
    for(t in 2:T){
        E_est[, t] <- E_est[, t-1] %*% p_est
    }
    return(sum((E[, -1] - E_est[, -1])^2))
}

## try optim first
start <- rep(0, M * (M - 1))
f0 <- optim(par = start, fn = ssq)

log_odds <- cbind(0, matrix(f0$par, nrow = M, ncol = M-1))
p_est <- exp(log_odds) / rowSums(exp(log_odds))

plot(c(p_true), c(p_est), xlim = c(0, 1), ylim = c(0, 1))
abline(c(0, 1))

## with a genetic algorithm
library(DEoptim)
niter <- 1e5 ## takes a while!

f1 <- DEoptim(ssq, lower = rep(-10, M * (M-1)), upper = rep(10, M * (M-1)),
              control = list(trace = FALSE, itermax = niter))

## check convergence visually
plot(f1)

dev.off(); par(ask = FALSE)

theta <- f1$optim$bestmem
log_odds <- cbind(0, matrix(theta, nrow = M, ncol = M-1))
p_est <- exp(log_odds) / rowSums(exp(log_odds))

plot(c(p_true), c(p_est), xlim = c(0, 1), ylim = c(0, 1))
abline(c(0, 1))

dev.off()
