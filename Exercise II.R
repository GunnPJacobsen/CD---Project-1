library(lmerTest)

rm(list = ls())


#---------------------------------------------------
# Sample Size 100 and Intraclass correlation 0.45
#---------------------------------------------------

ni <- 20 # sample size for each group
ng <- 5  # number of groups
mu = 15  # mean value

#We want intraclasscorrelation to be 0.45.
#We fix sigmaeps^2=1.
#Solve for sigmaa^2=9/11

sigma_e = sqrt(1)   #residual error variance or experimental error
sigma_a= sqrt(9/11)  # standard deviation of the random factor
sigma_e^2 #1
sigma_a^2 #9/11

#Check if it is 0.45
sigma_a^2/(sigma_a^2+sigma_e^2)# intraclass correlation (ratio of intraclass variance to total variance)

N=ni*ng #Total number of observations

set.seed(1)
df10045 <- data.frame( sample = 1:(ni*ng),
                       group = factor(rep(1:ng, each = ni)),
                       epsilon = rnorm(n = ni*ng, mean = 0, sd = sigma_e))

# epsilon: N(0,1)
df10045$ai<-0.0
set.seed(1)
ai<-rnorm(n=ng,mean = 0, sd = sigma_a)
for(igroup in 1:ng){
  for(iobs in 1:ni){
    df10045[iobs+ ni*(igroup-1),4]<-ai[igroup]
  }
}

## Generate response:
df10045$mu<-mu
df10045$y<- df10045$mu + df10045$ai + df10045$epsilon

#Create plots
str(df10045)
head(df10045,22)

(ymeans10045<-with(df10045,tapply(y, group, mean)))

par(mfrow=c(1,2))
with(df10045, stripchart(y ~ group, xlab='group', ylab='y', 
                         vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

points(1:5, ymeans10045, pch = 17, cex = 1.2, col = "black")
abline(h=mean(df10045$y), lty = 2)
title(main=list("Response (y), group means and overall mean \n N=100 and ICC=0.45",cex=1, col="black", font=3))

with(df10045, stripchart(ai ~ group, xlab='group', ylab='a', 
                         vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

abline(h=(mean(df10045$ai)), lty = 2, col= "black")
title(main=list("Random effects for the groups (ai) \n N=100 and ICC=0.45",
                cex=1.0, col="black", font=3))

##Fit model
m_lmer10045<-lmer(y~ 1 + (1|group), data=df10045)

summary(m_lmer10045)
#Mu=15.2258

#Estimated ICC
0.6716 / (0.6716 + 0.8363)
#0.4453876

## Confidence interval for mean:
confint(m_lmer10045)
#(Intercept) 14.4135451 16.038087

fixef(m_lmer10045)
ranef(m_lmer10045) #BLUPS estimators

library(lattice)
dotplot(ranef(m_lmer10045, condVar=TRUE), strip = FALSE)


#---------------------------------------------------
# Sample Size 1000 and Intraclass correlation 0.45
#---------------------------------------------------

ni <- 200 # sample size for each group

N=ni*ng #Total number of observations

set.seed(1)
df100045 <- data.frame( sample = 1:(ni*ng),
                        group = factor(rep(1:ng, each = ni)),
                        epsilon = rnorm(n = ni*ng, mean = 0, sd = sigma_e))

# epsilon: N(0,1)
df100045$ai<-0.0
set.seed(1)
ai<-rnorm(n=ng,mean = 0, sd = sigma_a)
for(igroup in 1:ng){
  for(iobs in 1:ni){
    df100045[iobs+ni*(igroup-1),4]<-ai[igroup]
  }
}


## Generate response:
df100045$mu<-mu
df100045$y<- df100045$mu + df100045$ai + df100045$epsilon

#Create plots
str(df100045)
head(df100045,22)

(ymeans100045<-with(df100045,tapply(y, group, mean)))

par(mfrow=c(1,2))
with(df100045, stripchart(y ~ group, xlab='group', ylab='y', 
                          vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

points(1:5, ymeans100045, pch = 17, cex = 1.2, col = "black")
abline(h=mean(df100045$y), lty = 2)
title(main=list("Response (y), group means and overall mean \n N=1000 and ICC=0.45",cex=1, col="black", font=3))

with(df100045, stripchart(ai ~ group, xlab='group', ylab='a', 
                          vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

abline(h=(mean(df100045$ai)), lty = 2, col= "black")
title(main=list("Random effects for the groups (ai) \n N=1000 and ICC=0.45",
                cex=1.0, col="black", font=3))

##Fit model
m_lmer100045<-lmer(y~ 1 + (1|group), data=df100045)

summary(m_lmer100045)
#Mu=15.1053

#Estimated ICC
0.6955 / (0.6955 + 1.0725)
# 0.3933824

## Confidence interval for mean:
confint(m_lmer100045)
#(Intercept) 14.3001926 15.910369


fixef(m_lmer100045)
ranef(m_lmer100045) #BLUPS estimators

library(lattice)
dotplot(ranef(m_lmer100045, condVar=TRUE), strip = FALSE)


#---------------------------------------------------
# Sample Size 100 and Intraclass correlation 0.95
#---------------------------------------------------

ni <- 20 # sample size for each group

#We want intraclasscorrelation to be 0.95.
#We fix sigmaeps^2=1.
#Solve for sigma_a^2=19

sigma_a= sqrt(19)  # standard deviation of the random factor
sigma_a^2 #19

#Check if it is 0.95
sigma_a^2/(sigma_a^2+sigma_e^2)# intraclass correlation (ratio of intraclass variance to total variance)

N=ni*ng #Total number of observations

set.seed(1)
df10095 <- data.frame( sample = 1:(ni*ng),
                       group = factor(rep(1:ng, each = ni)),
                       epsilon = rnorm(n = ni*ng, mean = 0, sd = sigma_e))

# epsilon: N(0,1)
df10095$ai<-0.0
set.seed(1)
ai<-rnorm(n=ng,mean = 0, sd = sigma_a)
for(igroup in 1:ng){
  for(iobs in 1:ni){
    df10095[iobs+ ni*(igroup-1),4]<-ai[igroup]
  }
}

## Generate response:
df10095$mu<-mu
df10095$y<- df10095$mu + df10095$ai + df10095$epsilon

#Create plots
str(df10095)
head(df10095,22)

(ymeans10095<-with(df10095,tapply(y, group, mean)))

par(mfrow=c(1,2))
with(df10095, stripchart(y ~ group, xlab='group', ylab='y', 
                         vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

points(1:5, ymeans10095, pch = 17, cex = 1.2, col = "black")
abline(h=mean(df10095$y), lty = 2)
title(main=list("Response (y), group means and overall mean \n N=100 and ICC=0.95",cex=1, col="black", font=3))

with(df10095, stripchart(ai ~ group, xlab='group', ylab='a', 
                         vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

abline(h=(mean(df10095$ai)), lty = 2, col= "black")
title(main=list("Random effects for the groups (ai) \n N=100 and ICC=0.95",
                cex=1.0, col="black", font=3))

##Fit model
m_lmer10095<-lmer(y~ 1 + (1|group), data=df10095)

summary(m_lmer10095)
#Mu=15.672

#Estimated ICC
17.2827 / (17.2827  +  0.8363)
#0.953844

## Confidence interval for mean:
confint(m_lmer10095)
#(Intercept) 11.669524 19.675198

fixef(m_lmer10095)
ranef(m_lmer10095) #BLUPS estimators

library(lattice)
dotplot(ranef(m_lmer10095, condVar=TRUE), strip = FALSE)

#---------------------------------------------------
# Sample Size 1000 and Intraclass correlation 0.95
#---------------------------------------------------

ni <- 200 # sample size for each group

sigma_a= sqrt(19)  # standard deviation of the random factor
sigma_a^2 #19

#Check if it is 0.95
sigma_a^2/(sigma_a^2+sigma_e^2)# intraclass correlation (ratio of intraclass variance to total variance)

N=ni*ng #Total number of observations

set.seed(1)
df100095 <- data.frame( sample = 1:(ni*ng),
                        group = factor(rep(1:ng, each = ni)),
                        epsilon = rnorm(n = ni*ng, mean = 0, sd = sigma_e))

# epsilon: N(0,1)
df100095$ai<-0.0
set.seed(1)
ai<-rnorm(n=ng,mean = 0, sd = sigma_a)
for(igroup in 1:ng){
  for(iobs in 1:ni){
    df100095[iobs+ ni*(igroup-1),4]<-ai[igroup]
  }
}

## Generate response:
df100095$mu<-mu
df100095$y<- df100095$mu + df100095$ai + df100095$epsilon

#Create plots
str(df100095)
head(df100095,22)

(ymeans100095<-with(df100095,tapply(y, group, mean)))

par(mfrow=c(1,2))
with(df100095, stripchart(y ~ group, xlab='group', ylab='y', 
                          vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

points(1:5, ymeans100095, pch = 17, cex = 1.2, col = "black")
abline(h=mean(df100095$y), lty = 2)
title(main=list("Response (y), group means and overall mean \n N=1000 and ICC=0.95",cex=1, col="black", font=3))

with(df100095, stripchart(ai ~ group, xlab='group', ylab='a', 
                          vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

abline(h=(mean(df100095$ai)), lty = 2, col= "black")
title(main=list("Random effects for the groups (ai) \n N=1000 and ICC=0.95",
                cex=1.0, col="black", font=3))

##Fit model
m_lmer100095<-lmer(y~ 1 + (1|group), data=df100095)

summary(m_lmer100095)
#Mu=15.552

#Estimated ICC
17.265 / (17.265  + 1.073)
# 0.9414876

## Confidence interval for mean:
confint(m_lmer100095)
#(Intercept) 11.5552062 19.548445


fixef(m_lmer100095)
ranef(m_lmer100095) #BLUPS estimators

library(lattice)
dotplot(ranef(m_lmer100095, condVar=TRUE), strip = FALSE)


#---------------------------------------------------
# Sample Size 100 and Intraclass correlation 0
#---------------------------------------------------

ni <- 20 # sample size for each group

#We want intraclasscorrelation to be 0.
#We fix sigmaeps^2=1.
#Solve for sigmaa^2=0

sigma_a= sqrt(0)  # standard deviation of the random factor
sigma_a^2 #0

#Check if it is 0
sigma_a^2/(sigma_a^2+sigma_e^2)# intraclass correlation (ratio of intraclass variance to total variance)

N=ni*ng #Total number of observations

set.seed(1)
df100 <- data.frame( sample = 1:(ni*ng),
                     group = factor(rep(1:ng, each = ni)),
                     epsilon = rnorm(n = ni*ng, mean = 0, sd = sigma_e))

# epsilon: N(0,1)
df100$ai<-0.0
set.seed(1)
ai<-rnorm(n=ng,mean = 0, sd = sigma_a)
for(igroup in 1:ng){
  for(iobs in 1:ni){
    df100[iobs+ ni*(igroup-1),4]<-ai[igroup]
  }
}

## Generate response:
df100$mu<-mu
df100$y<- df100$mu + df100$ai + df100$epsilon

#Create plots
str(df100)
head(df100,22)

(ymeans100<-with(df100,tapply(y, group, mean)))

par(mfrow=c(1,2))
with(df100, stripchart(y ~ group, xlab='group', ylab='y', 
                       vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

points(1:5, ymeans100, pch = 17, cex = 1.2, col = "black")
abline(h=mean(df100$y), lty = 2)
title(main=list("Response (y), group means and overall mean \n N=100 and ICC=0",cex=1, col="black", font=3))

with(df100, stripchart(ai ~ group, xlab='group', ylab='a', 
                       vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

abline(h=(mean(df100$ai)), lty = 2, col= "black")
title(main=list("Random effects for the groups (ai) \n N=100 and ICC=0",
                cex=1.0, col="black", font=3))


##Fit model
m_lmer100<-lmer(y~ 1 + (1|group), data=df100)

summary(m_lmer100)
#Mu=15.10889

#Estimated ICC
0 / (0  + 0.8068)
# 0

## Confidence interval for mean:
confint(m_lmer100)
#(Intercept) 14.932236 15.2855390


fixef(m_lmer100)
ranef(m_lmer100) #BLUPS estimators

library(lattice)
dotplot(ranef(m_lmer100, condVar=TRUE), strip = FALSE)

#---------------------------------------------------
# Sample Size 1000 and Intraclass correlation 0
#---------------------------------------------------

ni <- 200 # sample size for each group

N=ni*ng #Total number of observations

set.seed(1)
df1000 <- data.frame( sample = 1:(ni*ng),
                      group = factor(rep(1:ng, each = ni)),
                      epsilon = rnorm(n = ni*ng, mean = 0, sd = sigma_e))

# epsilon: N(0,1)
df1000$ai<-0.0
set.seed(1)
ai<-rnorm(n=ng,mean = 0, sd = sigma_a)
for(igroup in 1:ng){
  for(iobs in 1:ni){
    df1000[iobs+ ni*(igroup-1),4]<-ai[igroup]
  }
}

## Generate response:
df1000$mu<-mu
df1000$y<- df1000$mu + df1000$ai + df1000$epsilon

#Create plots
str(df1000)
head(df1000,22)

(ymeans1000<-with(df1000,tapply(y, group, mean)))

par(mfrow=c(1,2))
with(df1000, stripchart(y ~ group, xlab='group', ylab='y', 
                        vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

points(1:5, ymeans1000, pch = 17, cex = 1.2, col = "black")
abline(h=mean(df1000$y), lty = 2)
title(main=list("Response (y), group means and overall mean \n N=1000 and ICC=0",cex=1, col="black", font=3))

with(df1000, stripchart(ai ~ group, xlab='group', ylab='a', 
                        vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

abline(h=(mean(df1000$ai)), lty = 2, col= "black")
title(main=list("Random effects for the groups (ai) \n N=1000 and ICC=0",
                cex=1.0, col="black", font=3))

##Fit model
m_lmer1000<-lmer(y~ 1 + (1|group), data=df1000)

summary(m_lmer1000)
#Mu=14.98835

#Estimated ICC
0 / (0  + 1.071 )
#NA

## Confidence interval for mean:
confint(m_lmer1000)
#(Intercept) 14.9241787 15.0525250

fixef(m_lmer1000)
ranef(m_lmer1000) #BLUPS estimators

library(lattice)
dotplot(ranef(m_lmer1000, condVar=TRUE), strip = FALSE)

#--------------------
# Combined Plots
#--------------------

#PLOT RESPONSE
par(mfrow=c(2,3))

#N=100 ICC=0
with(df100, stripchart(y ~ group, xlab='group', ylab='y', 
                       vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

points(1:5, ymeans100, pch = 17, cex = 1.2, col = "black")
abline(h=mean(df100$y), lty = 2)
title(main=list("N=100 and ICC=0",cex=1, col="black", font=3))

#N=100 ICC=0.45
with(df10045, stripchart(y ~ group, xlab='group', ylab='y', 
                         vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

points(1:5, ymeans10045, pch = 17, cex = 1.2, col = "black")
abline(h=mean(df10045$y), lty = 2)
title(main=list("N=100 and ICC=0.45",cex=1, col="black", font=3))

#N=100 ICC=0.95
with(df10095, stripchart(y ~ group, xlab='group', ylab='y', 
                         vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

points(1:5, ymeans10095, pch = 17, cex = 1.2, col = "black")
abline(h=mean(df10095$y), lty = 2)
title(main=list("N=100 and ICC=0.95",cex=1, col="black", font=3))

#N=1000 ICC=0
with(df1000, stripchart(y ~ group, xlab='group', ylab='y', 
                        vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

points(1:5, ymeans1000, pch = 17, cex = 1.2, col = "black")
abline(h=mean(df1000$y), lty = 2)
title(main=list("N=1000 and ICC=0",cex=1, col="black", font=3))

#N=1000 ICC=0.45
with(df100045, stripchart(y ~ group, xlab='group', ylab='y', 
                          vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

points(1:5, ymeans100045, pch = 17, cex = 1.2, col = "black")
abline(h=mean(df100045$y), lty = 2)
title(main=list("N=1000 and ICC=0.45",cex=1, col="black", font=3))

#N=1000 ICC=0.95
with(df100095, stripchart(y ~ group, xlab='group', ylab='y', 
                          vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

points(1:5, ymeans100095, pch = 17, cex = 1.2, col = "black")
abline(h=mean(df100095$y), lty = 2)
title(main=list("N=1000 and ICC=0.95",cex=1, col="black", font=3))



#Plot RANDOM EFFECT
par(mfrow=c(2,3))

#N=100 ICC=0
with(df100, stripchart(ai ~ group, xlab='group', ylab='a', 
                       vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

abline(h=(mean(df100$ai)), lty = 2, col= "black")
title(main=list("Random effects for the groups (ai) \n N=100 and ICC=0",
                cex=1.0, col="black", font=3))



#N=100 ICC=0.45
with(df10045, stripchart(ai ~ group, xlab='group', ylab='a', 
                         vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

abline(h=(mean(df10045$ai)), lty = 2, col= "black")
title(main=list("Random effects for the groups (ai) \n N=100 and ICC=0.45",
                cex=1.0, col="black", font=3))

#N=100 ICC=0.95
with(df10095, stripchart(ai ~ group, xlab='group', ylab='a', 
                         vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

abline(h=(mean(df10095$ai)), lty = 2, col= "black")
title(main=list("Random effects for the groups (ai) \n N=100 and ICC=0.95",
                cex=1.0, col="black", font=3))


#N=1000 ICC=0
with(df1000, stripchart(ai ~ group, xlab='group', ylab='a', 
                        vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

abline(h=(mean(df1000$ai)), lty = 2, col= "black")
title(main=list("Random effects for the groups (ai) \n N=1000 and ICC=0",
                cex=1.0, col="black", font=3))


#N=1000 ICC=0.45
with(df100045, stripchart(ai ~ group, xlab='group', ylab='a', 
                          vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

abline(h=(mean(df100045$ai)), lty = 2, col= "black")
title(main=list("Random effects for the groups (ai) \n N=1000 and ICC=0.45",
                cex=1.0, col="black", font=3))


#N=1000 ICC=0.95
with(df100095, stripchart(ai ~ group, xlab='group', ylab='a', 
                          vertical=TRUE,cex=1.0,pch=16, las=1, col=2:11))

abline(h=(mean(df100095$ai)), lty = 2, col= "black")
title(main=list("Random effects for the groups (ai) \n N=1000 and ICC=0.95",
                cex=1.0, col="black", font=3))



#PLOT DOTPLOT
library(lattice)
library(gridExtra)
library(grid)

# Generate lattice dotplots and save them to a list
plots <- list(
  dotplot(ranef(m_lmer100, condVar = TRUE), strip = FALSE, sub = "N=100 and ICC=0"),
  dotplot(ranef(m_lmer10045, condVar = TRUE), strip = FALSE, sub = "N=100 and ICC=0.45"),
  dotplot(ranef(m_lmer10095, condVar = TRUE), strip = FALSE, sub = "N=100 and ICC=0.95"),
  dotplot(ranef(m_lmer1000, condVar = TRUE), strip = FALSE, sub = "N=1000 and ICC=0"),
  dotplot(ranef(m_lmer100045, condVar = TRUE), strip = FALSE, sub = "N=1000 and ICC=0.45"),
  dotplot(ranef(m_lmer100095, condVar = TRUE), strip = FALSE, sub = "N=1000 and ICC=0.95")
)

# Create a function to capture and convert each plot to a grob
grob_list <- lapply(plots, function(p) {
  grid.grabExpr(print(p))
})

# Arrange the grobs in a 2x3 layout
grid.arrange(grobs = grob_list, ncol = 3)
