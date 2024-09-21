#setwd("/Users/gunnjoensen/Documents/DTU/Correlated Data/Project 1/")

# File Description -------------------------------------------------------------
#
#   Gunn P. Jacobsen - s175799
#   Date: -
#   
#   Correlateded Data
#   Project 2
#-------------------------------------------------------------------------------


df <- read.csv("assignment1.csv", sep = ";")
df$ENV <- as.factor(df$ENV)


################################################################################
# Part 1
################################################################################


# Plots to explore the information in the quality response ---------------------
bp.ENV.YLD <- ggplot(df, aes(x=ENV, y=YLD)) + 
  geom_boxplot() +
  theme_classic()
bp.ENV.YLD

bp.GEN.YLD <- ggplot(df, aes(x=GEN, y=YLD)) + 
  geom_boxplot() +
  theme_classic()
bp.GEN.YLD

jitter.GEN.YLD <- ggplot(df, aes(x=GEN, y=YLD)) + 
  geom_jitter() +
  theme_classic()
jitter.GEN.YLD

jitter.ENV.YLD <- ggplot(df, aes(x=ENV, y=YLD)) + 
  geom_jitter() +
  theme_classic()
jitter.ENV.YLD



m1 <- lmer(YLD ~ GEN + (1|ENV), data = df, REML = FALSE)
summary(m1)

confint(m1)

m2 <- lm(YLD ~ GEN + ENV, data = df)
summary(m2)
confint(m2)


