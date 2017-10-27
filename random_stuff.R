library(R.utils)
library(rstan)
library(R2jags)
library(manipulate) # important
library(MCMCpack) #important
library(R2WinBUGS)

## Bayes in a nutshell: Three equivalent statements
## What we think about the world after seeing data =
##   What we thought about the world before seeing data x
##   Chance we'd see our data under different assumptions about the world
## Pr(world|data) = Pr(world) x Pr(data|world)
## Posterior = Prior x Likelihood
## A posterior is a probability distribution, used for inference.

## A simple example: Estimating a proportion from dichotomous 0/1 data

## For the prior distribution, choose "hyperparameters" that describe our 
##   belief about a quantity of interest (here, p) before seeing data.
## The Beta distribution is "conjugate" to the binomial likelihood, 
##   with hyperparameters alpha and beta.

p <- seq(from=0.005, to=0.995, by=0.005)

manipulate( # requires RStudio
  {plot(p, dbeta(p, alpha.hyper, beta.hyper), 
        col="blue", lwd=2, type="l", las=1, bty="n", 
        ylim=c(0, 8), ylab="density", 
        main="Beta prior distribution")
    polygon(c(p, rev(p)), c(dbeta(p, alpha.hyper, beta.hyper), 
                            rep(0, length(p))), col=rgb(0, 0, 1, 0.2), border=NA)}, 
  alpha.hyper=slider(0.1, 10, step=0.1, initial=1), 
  beta.hyper=slider(0.1, 10, step=0.1, initial=1))

## Now we observe some data
p.true <- 0.7
N <- 30
y <- rbinom(N, size=1, prob=p.true)
table(y)/N

## Likelihood of the data at each possible value of p
## (http://en.wikipedia.org/wiki/Bernoulli_distribution)
likelihood <- sapply(p, function(p) { prod(p^y * (1-p)^(1-y)) } )
plot(p, likelihood, lwd=2, las=1, bty="n", type="l")

## (To help with visibility)
like.rescale <- 4 * likelihood/max(likelihood)

## To get the posterior, multiply Prior x Likelihood at each value of p
## Or easier: Prior is conjugate, so posterior is Beta distributed with
##   alpha = alpha + k
##   beta = beta + N - k
## Where N = sample size, k = number of "successes".
## The prior is most influential when data are sparse.

manipulate(
  {plot(p, like.rescale, lwd=2, las=1, bty="n", 
        ylim=c(0,8), type="l", ylab="density", 
        main="Beta prior (blue) x Likelihood (black) = Beta posterior (red)")
    alpha.hyper.post <- alpha.hyper + sum(y)
    beta.hyper.post <- beta.hyper + N - sum(y)
    lines(p, dbeta(p, alpha.hyper, beta.hyper), col="blue", lwd=2)
    polygon(c(p, rev(p)), c(dbeta(p, alpha.hyper, beta.hyper), 
                            rep(0, length(p))), col=rgb(0, 0, 1, 0.2), border=NA)
    lines(p, dbeta(p, alpha.hyper.post, beta.hyper.post), col="red", lwd=2)
    polygon(c(p, rev(p)), c(dbeta(p, alpha.hyper.post, beta.hyper.post), 
                            rep(0, length(p))), col=rgb(1, 0, 0, 0.2), border=NA)
    lines(p, like.rescale, lwd=2)}, 
  alpha.hyper=slider(0.1, 10, step=0.1, initial=1), 
  beta.hyper=slider(0.1, 10, step=0.1, initial=1))



####################

dat <- data.frame(year = c(2016,2012,2008,2004,2000,1996,1992,1988,1984,1980,1976,1972,1968,1964,1960,1956,1952,1948),
                  gdp.growth = c(NA,1.3,1.3,2.6,8,7.1,4.3,5.2,7.1,-7.9,3,9.8,7,4.7,-1.9,3.2,0.4,7.5),
                  net.approval = c(NA,-0.8,-37,-0.5,19.5,15.5,-18,10,20,-21.7,5,26,-5,60.3,37,53.5,-27,-6),
                  two.terms = c(1,0,1,0,1,0,1,1,0,0,1,0,1,0,1,0,1,1),
                  incumbent.vote = c(NA,52.0,46.3,51.2,50.3,54.7,46.5,53.9,59.2,44.7,48.9,61.8,49.6,61.3,49.9,57.8,44.5,52.4))




breg <- MCMCregress(incumbent.vote ~ gdp.growth + net.approval + two.terms, dat)
summary(breg)
plot(breg)

















