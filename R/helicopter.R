## APTS Design of Experiments 2020 
## Simulated paper helicopter experiment
## Dave Woods, University of Southampton

library(apts.doe)

## See https://www.paperhelicopterexperiment.com for details about the experiment

## Set up the four factors and levels
w <- c(0.03, 0.09) # rotor width [0.03, 0.09]m
r <- c(0.07, 0.12) # rotor length [0.07, 0.12]m
t <- c(0.07, 0.12) # tail length [0.07, 0.12]m
d <- c(0.06, 0.12) # paper density [0.06, 0.12] kgm^-2

## Create the full factorial design
X <- expand.grid(w = w, r = r, t = t, d = d) # form the factorial design by taking all 16 combinations
X <- X[sample(1:16), ] # randomise the run order
y <- flighttime(w = X[, 1], r = X[, 2], t = X[, 3], d = X[, 4], theta1 = 1.9) # simulate the data
y

## Set up the four columns of X as factors, and recode the levels as -1, 1
X <- lapply(X, factor)
for(i in 1:4) contrasts(X[[i]])<- contr.twolevel() # function from apts.doe to recode levels
heli <- data.frame(X, y)

## fir the linear model, and get the coefficient
heli.lm <- lm(y ~ (.) ^ 4, data = heli)
coef(heli.lm)

## produce a half-normal plot to decide which effects are important (which estimated effects look like outliers)
library(FrF2)
par(pty = "s", mar = c(8, 4, 1, 2))
DanielPlot(heli.lm, main = "", datax = F, half = T, autolab = F)

## produce effects plots for important main effects and interactions
library(effects)
helip.lm <- lm(y ~ (.) ^ 3, data = heli)
plot(Effect("w", helip.lm), main = "", rug = F, aspect = 1)
plot(Effect("r", helip.lm), main = "", rug = F, aspect = 1)
plot(Effect("d", helip.lm), main = "", rug = F, aspect = 1)

plot(Effect(c("w", "r"), helip.lm), main = "", rug = F, x.var = "w")
plot(Effect(c("w", "d"), helip.lm), main = "", rug = F, x.var = "w")
plot(Effect(c("r", "d"), helip.lm), main = "", rug = F, x.var = "r")

plot(Effect(c("w", "r", "d"), helip.lm), main = "", rug = F, x.var = "w")

## what happens if we only look at a half-fraction?
X.full <- model.matrix(y ~ (.) ^ 4, data = heli) # construct the full model matrix

## pick out the half-fraction with x1x2x3x4 = 1; equivalent to assigning x4 = x1x2x3
heli.FF1 <- heli[X.full[, 16] == 1,]
heliFF1.lm <- lm(y ~ (.) ^ 4, data = heli.FF1)
coef(heliFF1.lm) # compare the estimated coefficients with the above for the full factorial - e.g. betahat1(full) = betahat1(frac) + betahat234(frac) 

## pick out the other half-fraction with x1x2x3x4 = -1; equivalent to assigning x4 = -x1x2x3
heli.FF2 <- heli[X.full[, 16] == -1,]
heliFF2.lm <- lm(y ~ (.) ^ 4, data = heli.FF2)
coef(heliFF2.lm) # compare the estimated coefficients with the above for the full factorial - e.g. betahat1(full) = betahat1(frac) - betahat234(frac) 

## now pick out the runs corresponding to a 12 run Plackett-Burman design
pb12 <- pb(12, 4)
pb12 <- sapply(pb12, function(x) as.numeric(as.character(x)))
heli.PB <- heli[match(data.frame(t(pb12)),data.frame(t(X.full[,2:5]))), ]

## fit the model, and get the coefficients
heliPB.lm <- lm(y ~ (.) ^ 4, data = heli.PB)
coef(heliPB.lm)
## and then the alias matrix; notice all the partial aliasing between the main effect of a factor and all the two-factor interactions not involving that factor
X.PB<-model.matrix(heliPB.lm)
A <- Amatrix(X.PB[, 1:5], X.PB[, 6:16]) # Amatrix is a simple function from apts.doe for calculating the alias matrix
round(A, 2)
