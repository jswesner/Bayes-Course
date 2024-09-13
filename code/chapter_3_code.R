## R code 3.1
Pr_Positive_Vampire <- 0.95
Pr_Positive_Mortal <- 0.01
Pr_Vampire <- 0.001
Pr_Positive <- Pr_Positive_Vampire * Pr_Vampire +
  Pr_Positive_Mortal * ( 1 - Pr_Vampire )
( Pr_Vampire_Positive <- Pr_Positive_Vampire*Pr_Vampire / Pr_Positive )

## R code 3.2
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prob_p <- rep( 1 , 1000 )
prob_data <- dbinom( 600 , size=900 , prob=p_grid )
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

## R code 3.3
samples <- sample( p_grid , prob=posterior , size=1e5 , replace=TRUE )

## R code 3.4
plot( samples )

## R code 3.5
library(rethinking)
dens( samples , xlim = c(0, 1))

## R code 3.6
# add up posterior probability where p < 0.5
sum( posterior[ p_grid < 0.5 ] )

## R code 3.7
sum( samples < 0.5 ) / 1e4

## R code 3.8
sum( samples > 0.5 & samples < 0.75 ) / 1e4

## R code 3.9
quantile( samples , 0.8 )

## R code 3.10
quantile( samples , c( 0.1 , 0.9 ) )

## R code 3.11
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep(1,1000)
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )

## R code 3.12
PI( samples , prob=0.5 )

## R code 3.13
HPDI( samples , prob=0.5 )

## R code 3.14
p_grid[ which.max(posterior) ]

## R code 3.15
chainmode( samples , adj=0.01 ) # same as which.max for posteriors

## R code 3.16
mean( samples )
median( samples )

## R code 3.17
sum( posterior*abs( 0.5 - p_grid ) )

## R code 3.18

loss <- sapply( p_grid , function(d) sum( posterior*abs( d - p_grid ) ) )  # d is the p_grid in this case. Confusing. It cycles through each p_grid value and calculates loss...also using p_grid.

## R code 3.19
p_grid[ which.min(loss) ]

## R code 3.20
dbinom( 0:2 , size=2 , prob=0.7 )

## R code 3.21
rbinom( 1 , size=2 , prob=0.7 )

## R code 3.22
rbinom( 10 , size=2 , prob=0.7 )

## R code 3.23
dummy_w <- rbinom( 1e5 , size=2 , prob=0.7 )
table(dummy_w)/1e5

## R code 3.24
dummy_w <- rbinom( 1e5 , size=9 , prob=0.7 )
simplehist( dummy_w , xlab="dummy water count" )

## R code 3.25
w <- rbinom( 1e4 , size=9 , prob=0.6 )

## R code 3.26
w <- rbinom( 1e4 , size=9 , prob=samples )

## R code 3.27
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )



# Homework ----------------------------------------------------------------

#3e1
sum(samples<0.2)/1e4
#3e2
sum(samples>0.8)/1e4
#3e3
sum(samples>0.2 & samples<0.8)/1e4
#3e4
quantile(samples, 0.2)
#3e5
quantile(samples, 0.8)
#3e6
HPDI(samples, prob = 0.66)
#3e6
PI(samples, prob = 0.66)


#3m1
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
#3m2
HPDI(samples, prob = 0.9)
#3m3
sims = rbinom(1e4, size = 15, prob = samples)
sum(sims == 8)/1e4
#3m4
median(dbinom(6, 9, prob = samples))


#3m5
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- ifelse(p_grid <= 0.5, 0, 1)
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
#3m5
HPDI(samples, prob = 0.9)
#3m5
sims = rbinom(1e4, size = 15, prob = samples)
sum(sims == 8)/1e4
#3m5
median(dbinom(6, 9, prob = samples))


#3m6
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- ifelse(p_grid <= 0.5, 0, 1)
ntosses = 3000
nwaters = 0.7*ntosses
likelihood <- dbinom( nwaters , size=ntosses , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
#3m5
PI(samples, prob = 0.99)


## R code 3.28
birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,
            0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,
            1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,
            1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
            1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,
            0,0,0,1,1,1,0,0,0,0)

## R code 3.29
library(rethinking)
data(homeworkch3)

## R code 3.30
sum(birth1) + sum(birth2)


