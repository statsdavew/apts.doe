## APTS Design of Experiments 2019
## R code extracted from the notes
## Dave Woods, University of Southampton

options(warnPartialMatchArgs=FALSE)

## ----prelim--------------------------------------------------------------
library(devtools)
install_github("statsdavew/apts.doe", quiet = T)
library(apts.doe)

## ----motivation, fig.align='center'--------------------------------------
n <- 50
eff <- function(n1) 1 - ((2 * n1 - n) / n)^2
curve(eff, from = 0, to = n, ylab = "Eff", xlab = expression(n[1]))

## ------------------------------------------------------------------------
with(cirfab, cirfab[order(x1, x2, x3, x4), ])

## ------------------------------------------------------------------------
cirfab.lm <- lm(ybar ~ (.) ^ 2, data = cirfab)
coef(cirfab.lm)

## ----fig.show='hold', out.width="45%", echo = F--------------------------
library(effects)
plot(Effect("x1", cirfab.lm), main = "", rug = F, ylim = c(13.8, 14.5))
plot(Effect("x2", cirfab.lm), main = "", rug = F, ylim = c(13.8, 14.5))
plot(Effect("x3", cirfab.lm), main = "", rug = F, ylim = c(13.8, 14.5))
plot(Effect("x4", cirfab.lm), main = "", rug = F, ylim = c(13.8, 14.5))

## ---- echo = T-----------------------------------------------------------
plot(Effect(c("x3", "x4"), cirfab.lm), main = "", rug = F, ylim = c(13.5, 15), 
     x.var = "x4")

## ---- fig.show='hold', out.width = "100%"--------------------------------
library(FrF2)
par(pty = "s", mar = c(8, 4, 1, 2))
DanielPlot(cirfab.lm, main = "", datax = F, half = T)

## ------------------------------------------------------------------------
cirfab2.lm <- lm(ybar ~ (.), data = cirfab)
coef(cirfab.lm)
coef(cirfab2.lm)
cbind(sigma1 = summary(cirfab.lm)$sigma, df1 = summary(cirfab.lm)$df[2],  
      sigma2 = summary(cirfab2.lm)$sigma, df2 = summary(cirfab2.lm)$df[2])

## ----bact----------------------------------------------------------------
bact.design <- FrF2(8, 5, factor.names = paste0("x", 1:5), 
     generators = list(c(1, 3), c(2, 3)), randomize = F, alias.info = 3)
bact.design

## ----bact2---------------------------------------------------------------
model.matrix(~ (x1 + x2 + x3) ^ 3, bact.design[, 1:3])[, ]

## ----bact3---------------------------------------------------------------
design.info(bact.design)$aliased 

## ----alias---------------------------------------------------------------
ff.alias <- alias(y ~ (.)^2, data = data.frame(bact.design, y = vector(length = 8)))
ff.alias$Complete

## ----lmNA----------------------------------------------------------------
bact.lm <- lm(yB ~ (x1 + x2 + x3 + x4 + x5)^2, data = bact)
summary(bact.lm)

## ----pb------------------------------------------------------------------
pb.design <- pb(12, factor.names = paste0("x", 1:11))
pb.design

## ----pbalias-------------------------------------------------------------
pb.alias <- alias(y ~ (.)^2, data = data.frame(pb.design, y = vector(length = 12)))
head(pb.alias$Complete, n = 15)

## ----ssd-----------------------------------------------------------------
ssd

## ----ssdcor, out.width="75%", fig.align='center'-------------------------
library(fields)
par(mar=c(8,2,0,0))
image.plot(1:16,1:16, cor(ssd), zlim = c(-1, 1), xlab = "Factors", 
           ylab = "", asp = 1, axes = F)
axis(1, at = seq(2, 16, by = 2), line = .5)
axis(2, at = seq(2, 16, by = 2), line = -5)

## ----compex, out.width = '65%', fig.align = 'center'---------------------
comp <- function(x, theta, D = 400) {
	mu <- exp(-theta[1] * x) - exp(-theta[2] * x)
	c <- (D / theta[3]) * (theta[2]) / (theta[2] - theta[1])
	c * mu }
theta <- c(.1, 1, 20)
M <- 100
par(mar = c(6, 4, 0, 1) + .1)
temp <-lapply(1:M, function(l) {
  thetat <- rlnorm(3,log(theta),rep(0.05,3))
  curve(comp(x, theta = thetat), from = 0, to = 24, ylab = "Expected concentration", 
        xlab = "Time", ylim = c(0, 20), xlim = c(0, 24), add = l!=1) })

## ----simplelogistic, fig.align='center'----------------------------------
rho <- function(x, beta0 = 0, beta1 = 1) {
  eta <- beta0 + beta1 * x
  1 / (1 + exp(-eta))
}
par(mar = c(8, 4, 1, 2) + 0.1)
curve(rho, from = -5, to = 5, ylab = expression(rho), xlab = expression(italic(x)), cex.lab = 1.5, 
      cex.axis = 1.5, ylim = c(0, 1), lwd = 2)

## ----simplelogisticinfo--------------------------------------------------
Minfo <- function(xi, beta0 = 0, beta1 = 1) {
  X <- cbind(c(1, 1), xi)
  v <- function(x) rho(x, beta0, beta1) * (1 - rho(x, beta0, beta1))
  W <- diag(c(v(xi[1]), v(xi[2])))
  t(X) %*% W %*% X
}
Dcrit <- function(xi, beta0 = 0, beta1 = 1) {
  d <- det(Minfo(xi, beta0, beta1))
  ifelse(is.nan(d), -Inf, d)
}

## ----simplelogisticdesign1-----------------------------------------------
dopt <- optim(par = c(-1, 1), Dcrit, control = list(fnscale = -1))
xi.opt1 <- dopt$par
xi.opt1

## ----simplogplot1, fig.align='center', echo = F--------------------------
plot.localopt <- function(xi, b0, b1) {
  par(mar = c(8, 4, 1, 2) + 0.1)
  curve(rho(x, beta0 = b0, beta1 = b1), from = -5, to = 5, ylab = expression(rho), xlab = expression(italic(x)), cex.lab = 1.5, 
      cex.axis = 1.5, ylim = c(0, 1), lwd = 2)
  points(c(xi[1], xi[2]), c(-0.02, -0.02), pch = 16, cex = 2, col = "darkblue")
  lines(c(xi[1], xi[1]), c(-0.02, rho(xi[1], beta0 = b0, beta1 = b1)), lty = 2, lwd = 2)
  lines(c(xi[2], xi[2]), c(-0.02, rho(xi[2], beta0 = b0, beta1 = b1)), lty = 2, lwd = 2)
  lines(c(-5, xi[1]), c(rho(xi[1], beta0 = b0, beta1 = b1), rho(xi[1], beta0 = b0, beta1 = b1)),lty = 2, lwd = 2)
  lines(c(-5, xi[2]), c(rho(xi[2], beta0 = b0, beta1 = b1), rho(xi[2], beta0 = b0, beta1 = b1)), lty = 2, lwd = 2)
}
plot.localopt(xi.opt1, 0, 1)

## ----simplelogisticdesign2-----------------------------------------------
dopt <- optim(par = c(-1, 1), Dcrit, control = list(fnscale = -1), beta1 = 2)
xi.opt2 <- dopt$par
xi.opt2

## ----simplogplot2, fig.align='center', echo = F--------------------------
plot.localopt(xi.opt2, 0, 2)

## ----simplelogisticdesign.5----------------------------------------------
dopt <- optim(par = c(-1, 1), Dcrit, control = list(fnscale = -1), beta1 = .5)
xi.opt3 <- dopt$par
xi.opt3

## ----simplogplot3, fig.align='center', echo = F--------------------------
plot.localopt(xi.opt3, 0, .5)

## ----evald---------------------------------------------------------------
(Dcrit(xi.opt3, beta1 = 2) / Dcrit(xi.opt2, beta1 = 2)) ^ (1 / 2)

## ----simplelogwrong, fig.align='center', echo = F------------------------
 par(mar = c(8, 4, 1, 2) + 0.1)
  curve(rho(x, beta0 = 0, beta1 = 2), from = -5, to = 5, ylab = expression(rho), xlab = expression(italic(x)), cex.lab = 1.5, 
      cex.axis = 1.5, ylim = c(0, 1), lwd = 2)
  xi <- xi.opt2
  points(c(xi[1], xi[2]), c(-0.02, -0.02), pch = 16, cex = 2, col = "darkblue")
  lines(c(xi[1], xi[1]), c(-0.02, rho(xi[1], beta0 = 0, beta1 = 2)), lty = 2, lwd = 2)
  lines(c(xi[2], xi[2]), c(-0.02, rho(xi[2], beta0 = 0, beta1 = 2)), lty = 2, lwd = 2)
  lines(c(-5, xi[1]), c(rho(xi[1], beta0 = 0, beta1 = 2), rho(xi[1], beta0 = 0, beta1 = 2)),lty = 2, lwd = 2)
  lines(c(-5, xi[2]), c(rho(xi[2], beta0 = 0, beta1 = 2), rho(xi[2], beta0 = 0, beta1 = 2)), lty = 2, lwd = 2)
  xi <- xi.opt3
  points(c(xi[1], xi[2]), c(-0.02, -0.02), pch = 16, cex = 2, col = "darkred")
  lines(c(xi[1], xi[1]), c(-0.02, rho(xi[1], beta0 = 0, beta1 = 2)), lty = 2, lwd = 2)
  lines(c(xi[2], xi[2]), c(-0.02, rho(xi[2], beta0 = 0, beta1 = 2)), lty = 2, lwd = 2)
  lines(c(-5, xi[1]), c(rho(xi[1], beta0 = 0, beta1 = 2), rho(xi[1], beta0 = 0, beta1 = 2)),lty = 2, lwd = 2)
  lines(c(-5, xi[2]), c(rho(xi[2], beta0 = 0, beta1 = 2), rho(xi[2], beta0 = 0, beta1 = 2)), lty = 2, lwd = 2)

## ----acebayesglmutil-----------------------------------------------------
library(acebayes)
prior <- list(support = matrix(c(0, 0, .5, 2), nrow = 2))
logreg.util <- utilityglm(formula = ~ x, family = binomial, prior = prior)$utility
BDcrit <- function(xi) logreg.util(data.frame(x = xi))
bdopt <- optim(par = c(-1, 1), BDcrit, control = list(fnscale = -1))
bdopt$par

## ----bayesdoptplot, echo = F, out.width='50%', fig.align = 'center'------
 par(mar = c(8, 4, 1, 2) + 0.1)
  curve(rho(x, beta0 = 0, beta1 = .5), from = -5, to = 5, ylab = expression(rho), xlab = expression(italic(x)), cex.lab = 1.5, 
      cex.axis = 1.5, ylim = c(0, 1), lwd = 2)
  curve(rho(x, beta0 = 0, beta1 = 2), lty = 3, from = -5, to = 5, ylab = expression(rho), xlab = expression(italic(x)), cex.lab = 1.5, 
      cex.axis = 1.5, ylim = c(0, 1), lwd = 2, add = T)
  xi <- xi.opt2
  points(c(xi[1], xi[2]), c(-0.02, -0.02), pch = 16, cex = 2, col = "darkblue")
  lines(c(xi[1], xi[1]), c(-0.02, rho(xi[1], beta0 = 0, beta1 = 2)), lty = 2, lwd = 2)
  lines(c(xi[2], xi[2]), c(-0.02, rho(xi[2], beta0 = 0, beta1 = 2)), lty = 2, lwd = 2)
  lines(c(-5, xi[1]), c(rho(xi[1], beta0 = 0, beta1 = 2), rho(xi[1], beta0 = 0, beta1 = 2)),lty = 2, lwd = 2)
  lines(c(-5, xi[2]), c(rho(xi[2], beta0 = 0, beta1 = 2), rho(xi[2], beta0 = 0, beta1 = 2)), lty = 2, lwd = 2)
  xi <- xi.opt3
  points(c(xi[1], xi[2]), c(-0.02, -0.02), pch = 16, cex = 2, col = "darkred")
  lines(c(xi[1], xi[1]), c(-0.02, rho(xi[1], beta0 = 0, beta1 = .5)), lty = 2, lwd = 2)
  lines(c(xi[2], xi[2]), c(-0.02, rho(xi[2], beta0 = 0, beta1 = .5)), lty = 2, lwd = 2)
  lines(c(-5, xi[1]), c(rho(xi[1], beta0 = 0, beta1 = .5), rho(xi[1], beta0 = 0, beta1 = .5)),lty = 2, lwd = 2)
  lines(c(-5, xi[2]), c(rho(xi[2], beta0 = 0, beta1 = .5), rho(xi[2], beta0 = 0, beta1 = .5)), lty = 2, lwd = 2)
  xi <- bdopt$par
  points(c(xi[1], xi[2]), c(-0.02, -0.02), pch = 16, cex = 2, col = "gray")

## ----glmMCutil-----------------------------------------------------------
priorMC <- function(B) cbind(rep(0, B), runif(n = B, min = .5, max = 2))
logreg.utilSIG <- utilityglm(formula = ~ x, family = binomial, prior = priorMC, criterion = "SIG")$utility
BDcritSIG <- function(xi, B = 1000) mean(logreg.utilSIG(data.frame(x = xi), B))
bdoptSIG <- optim(par = c(-1, 1), BDcritSIG, control = list(fnscale = -1))
bdoptSIG$par
bdopt$par

## ----compsmooth, fig.align = 'center', out.width = "60%"-----------------
library(DiceKriging)
library(DiceDesign)
n <- 10; x1<- -0.583; x2 <- 2 * maximinSA_LHS(lhsDesign(n, 1)$design)$design- 1
u <- NULL; for(i in 1:n) u[i] <- mean(utilcomp15sig(c(x1, x2[i]), B = 1000))
par(mar = c(4, 4, 2, 2) + 0.1)
plot(12 * (x2 + 1), u, xlab = expression(x[2]), ylab = "Approx. expected SIG", xlim = c(0, 24), 
     ylim = c(0, 2), pch = 16, cex = 1.5); abline(v = 12 * (x1 + 1), lwd = 2)
usmooth <- km(design = 12 * (x2 + 1), response = u, nugget = 1e-3, control = list(trace = F))
xgrid <- matrix(seq(0, 24, l = 1000), ncol = 1); pred <- predict(usmooth, xgrid, type = "SK")$mean
lines(seq(0, 24, l = 1000), pred, col = "blue", lwd = 2); abline(v = xgrid[which.max(pred), ], lty = 2)

## ----MFLutil-------------------------------------------------------------
## set up prior
priorMFL <- function(B) {
  b0 <- runif(B, -3, 3)
  b1 <- runif(B, 4, 10)
  b2 <- runif(B, 5, 11)
  b3 <- runif(B, -6, 0)
  b4 <- runif(B, -2.5, 2.5)
  cbind(b0, b1, b2, b3, b4)
}
## define the utility function
MFL.utilSIG <- utilityglm(formula = ~ x1 + x2 + x3 + x4, family = binomial, prior = priorMFL, 
                          criterion = "SIG")$utility
## starting design with n=18 runs, on [-1, 1]
d <- 2 * randomLHS(18, 4) - 1
colnames(d) <- paste0("x", 1:4)
## approximate expected utility for starting design
mean(MFL.utilSIG(d, 1000))

## ----MFLace, eval = F----------------------------------------------------
## ## not run - quite computationally expensive
## MLF.ace <- ace(utility = MFL.utilSIG, start.d = d, progress = T)

## ----MFLdesign, fig.align = 'center'-------------------------------------
pairs(optdeslrsig(18), pch = 16, 
      labels=c(expression(x[1]), expression(x[2]), expression(x[3]), expression(x[4])), cex = 2)

## ----MFHLdesign, fig.align = 'center'------------------------------------
pairs(optdeshlrsig(18), pch = 16, 
      labels=c(expression(x[1]), expression(x[2]), expression(x[3]), expression(x[4])),
      col = c("black", "red", "blue")[rep(1:3, rep(6, 3))], cex = 2)

## ----ebmex, fig.align = 'center'-----------------------------------------
## design and data are in 'ebm'
library(akima)
fld <- interp(x = ebm$x1, y = ebm$x2, z = ebm$y)
filled.contour(x = fld$x, y = fld$y, z = fld$z, asp = 1)

## ----simplelhd, out.width = "50%", fig.align='center'--------------------
LH <- function(n = 3, d = 2) {
	D <- NULL
	for(i in 1:d) D <- cbind(D, sample(1:n, n))
	D 
}
set.seed(4)
par(mar=c(5,6,2,4)+0.1, pty = "s")
plot((LH() -.5)/ 3, xlim = c(0, 1), ylim = c(0, 1), pty = "s", xlab = expression(x[1]), 
     ylab = expression(x[2]), pch = 16, cex.lab = 2, cex.axis = 2, cex = 2)
abline(v = c(0, 1/3, 2/3, 1), lty = 2)
abline(h = c(0, 1/3, 2/3, 1), lty = 2)

## ----lhs, fig.show='hold', eval = F--------------------------------------
library(DiceDesign)
lhs.d <- lhsDesign(9, 2)
plot(lhs.d$design, xlim = c(0, 1), ylim = c(0, 1), pty = "s", xlab = expression(x[1]),
     ylab = expression(x[2]), pch = 16, cex.lab = 2, cex.axis = 2, cex = 2,
     main = "random", cex.main = 2)
abline(v = seq(0, 9) / 9, lty = 2)
abline(h = seq(0, 9) / 9, lty = 2)

discrep.d <- discrepSA_LHS(lhs.d$design, criterion = "C2")
plot(discrep.d$design, xlim = c(0, 1), ylim = c(0, 1), pty = "s", xlab = expression(x[1]),
     ylab = expression(x[2]), pch = 16, cex.lab = 2, cex.axis = 2, cex = 2,
     main = "discrepancy", cex.main = 2)
abline(v = seq(0, 9) / 9, lty = 2)
abline(h = seq(0, 9) / 9, lty = 2)

maximin.d <- maximinSA_LHS(discrep.d$design)
plot(maximin.d$design, xlim = c(0, 1), ylim = c(0, 1), pty = "s", xlab = expression(x[1]),
     ylab = expression(x[2]), pch = 16, cex.lab = 2, cex.axis = 2, cex = 2,
     main = "maximin", cex.main = 2)
abline(v = seq(0, 9) / 9, lty = 2)
abline(h = seq(0, 9) / 9, lty = 2)

## ----ebmdesign, fig.align="center"---------------------------------------
par(mar=c(5,6,2,4)+0.1, pty = "s")
plot(ebm[, 2:3], xlim = c(-1, 1), ylim = c(-1, 1), pty = "s", xlab = expression(x[1]), 
     ylab = expression(x[2]), pch = 16, asp = 1)
abline(v = 2 * seq(0:20) / 20 - 1, lty = 2)
abline(h = 2 * seq(0:20) / 20 - 1, lty = 2)

## ----gpsimpex2, echo = F, fig.align='center'-----------------------------
library(DiceDesign)
library(DiceKriging)
xi <- lhsDesign(6, 1)$design
y <- sin(2 * pi * xi)
gp <- km(design = xi, response = y, control = list(trace = F))
xs <- sort(c(seq(0, 1, length = 100), xi))
gpp <- predict(gp, newdata = xs, type = "SK")

plot(xs, gpp$mean, ylim = c(-2, 2), type = "l", col = "red", lwd = 3, ylab = "", xlab = "x")
points(xi, y, pch = 4,lwd = 4, col = "blue")
lines(xs, gpp$upper95, lty = 2, lwd = 3)
lines(xs, gpp$lower95, lty = 2, lwd = 3)
legend(x = "topright", legend = c("posterior mean of g", "posterior quantiles for g", 
                                  expression(paste("observed data ", g(x[i])))), lty = c(1, 2, NA), 
       pch = c(NA, NA, 4), lwd = c(4, 4, 4), col = c("red", "black", "blue"))

## ----gpebm---------------------------------------------------------------
gpebm <- km(formula = ~., design = ebm[, 2:3], response = ebm[, 1], control = list(trace = F))
gpebm

## ----gpebm2, fig.show="hold", out.width="50%"----------------------------
xs1 <- sort(c(seq(-1, 1, length = 10), ebm[, 2]))
xs2 <- sort(c(seq(-1, 1, length = 10), ebm[, 3]))
xs <- expand.grid(x1 = xs1, x2 = xs2)
gppebm <- predict(gpebm, newdata = xs, type = "UK")
filled.contour(x = xs1, y = xs2, z = matrix(gppebm$mean, nrow = length(xs1)))
filled.contour(x = xs1, y = xs2, z = matrix(gppebm$sd, nrow = length(xs1)))

## ----gpEIa, echo = F, fig.align='center'---------------------------------
xi <- matrix(c(0.1, 0.8, 0.9), ncol = 1)
fn <- function(x) sin(2 * pi * x)
y <- fn(xi)
gp <- km(design = xi, response = y, control = list(trace = F))
xs <- sort(c(seq(0, 1, length = 100), xi))
gpp <- predict(gp, newdata = xs, type = "SK")

plot(xs, gpp$mean, ylim = c(-2, 2), type = "l", col = "red", lwd = 3, ylab = "", xlab = "x")
points(xi, y, pch = 4,lwd = 4, col = "blue")
lines(xs, gpp$upper95, lty = 2, lwd = 3)
lines(xs, gpp$lower95, lty = 2, lwd = 3)

## ----gpEI1---------------------------------------------------------------
library(DiceOptim)
xin <- max_EI(model = gp, lower = 0, upper = 1)$par

## ----gpEI1a, fig.show="hold", out.width="50%"----------------------------
EI(xin, gp)
plot(xs, gpp$mean, ylim = c(-2, 2), type = "l", col = "red", lwd = 3, ylab = "", xlab = "x", cex.lab = 2)
points(xi, y, pch = 4,lwd = 4, col = "blue")
lines(xs, gpp$upper95, lty = 2, lwd = 3)
lines(xs, gpp$lower95, lty = 2, lwd = 3)
abline(v = xin)
plot(xs, sapply(xs, EI, model = gp), type = "l", lwd = 3, ylab = "", xlab = "x", cex.lab = 2)

## ----gpEI2---------------------------------------------------------------
xi <- rbind(xi, xin)
y <- c(y, fn(xin))
gp2 <- km(design = xi, response = y, control = list(trace = F))
xin <- max_EI(model = gp2, lower = 0, upper = 1, control = list(trace = F))$par

## ----gpEI2a, fig.show="hold", out.width="50%"----------------------------
EI(xin, gp2)
xs <- sort(c(seq(0, 1, length = 100), xi))
gpp <- predict(gp2, newdata = xs, type = "SK")
plot(xs, gpp$mean, ylim = c(-2, 2), type = "l", col = "red", lwd = 3, ylab = "", xlab = "x", cex.lab = 2)
points(xi, y, pch = 4,lwd = 4, col = "blue")
lines(xs, gpp$upper95, lty = 2, lwd = 3)
lines(xs, gpp$lower95, lty = 2, lwd = 3)
abline(v = xin)
plot(xs, sapply(xs, EI, model = gp2), type = "l", lwd = 3, ylab = "", xlab = "x", cex.lab = 2)

## ----gpEI3---------------------------------------------------------------
xi <- rbind(xi, xin)
y <- c(y, fn(xin))
gp3 <- km(design = xi, response = y, control = list(trace = F))
xin <- max_EI(model = gp3, lower = 0, upper = 1, control = list(trace = F))$par

## ----gpEI3a, fig.show="hold", out.width="50%"----------------------------
EI(xin, gp3)
xs <- sort(c(seq(0, 1, length = 100), xi))
gpp <- predict(gp3, newdata = xs, type = "SK")
plot(xs, gpp$mean, ylim = c(-2, 2), type = "l", col = "red", lwd = 3, ylab = "", xlab = "x", cex.lab = 2)
points(xi, y, pch = 4,lwd = 4, col = "blue")
lines(xs, gpp$upper95, lty = 2, lwd = 3)
lines(xs, gpp$lower95, lty = 2, lwd = 3)
abline(v = xin)
plot(xs, sapply(xs, EI, model = gp3), type = "l", lwd = 3, ylab = "", xlab = "x", cex.lab = 2)

## ----EBM sensitivity-----------------------------------------------------
library(sensitivity)
d <- 2; n <- 1000
X1 <- data.frame(matrix(runif(d * n), nrow = n))
X2 <- data.frame(matrix(runif(d * n), nrow = n))
colnames(X1) <- colnames(ebm)[2:3]; colnames(X2) <- colnames(ebm)[2:3]
res <- sobolGP(model = gpebm, type = "UK", MCmethod = "sobol2002", X1, X2)
res

