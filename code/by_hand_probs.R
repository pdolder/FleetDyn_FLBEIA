##-----------------
## Calculate probabilities by hand
## CM, PD: 14/03/2018
## using model m1 as first example
## need to run to m1 fit in "RUM_fit.R"
##-----------------

## probabilities from mlogit
p_hat <- fitted(m1, outcome = FALSE)

## coefficient vector
beta <- as.matrix(coef(m1))

## model matrix
X <- model.matrix(m1)

## linear predictor long
eta_long <- X %*% beta

## linear predictor wide
eta_wide <- matrix(eta_long, ncol = 12, byrow = TRUE)
names(eta_wide) <- toupper(letters[1:12])

## convert to a probability
own_p_hat <- exp(eta_wide) / rowSums(exp(eta_wide))

## check
range(p_hat - own_p_hat)

## note some large numbers exponentiated to guard against computational infinities perhaps use Brobdingnag
