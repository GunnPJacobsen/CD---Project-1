rm(list = ls())

############## Load the packages ##############


required_packages = c("lmerTest", "emmeans", "xtable", "performance","texreg","sjPlot")
for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
        library(pkg, character.only = TRUE)
    }
}




############## Load the data ##############
data = read.csv("assignment1.csv",sep=';')
data$ENV = factor(data$ENV)
data$GEN = factor(data$GEN)


summary(data)


############## Exploratory analysis of the data ##############

# Plot boxplots of yield by environment and genotype
png(filename = "figures/boxplots.png", width = 800, height = 600)
par(mfrow=c(1,2))
boxplot(YLD ~ENV, data=data, main="Yield by Environment", xlab="Environment", ylab="Yield")
boxplot(YLD ~GEN, data=data, main="Yield by Genotype", xlab="Genotype", ylab="Yield")
dev.off()

# Plot interaction plot of yield by environment and genotype
png(filename = "figures/interaction_plot.png", width = 800, height = 600)
par(mfrow=c(1,2))
with(data, {
  interaction.plot(ENV, GEN, YLD, legend=TRUE, 
                   bty="n", col=2:11, xtick = TRUE,main="Interaction of Yield by Environment and Genotype", xlab = "Environment", ylab = "Yield")
  interaction.plot(GEN, ENV, YLD, legend=TRUE, 
                   bty="n", col=2:11, xtick = TRUE, main="Interaction of Yield by Genotype and Environment", xlab = "Genotype", ylab = "Yield")
})
dev.off()



############## Linear mixed model ##############
m1 = lmer(YLD ~ GEN + (1|ENV), REML=FALSE, data = data)



# texreg(list(m1),
#     include.rsquared = FALSE,     
#     include.ci = TRUE,  
#     ci.force = TRUE,                      
#     ci.level = 0.95,
#     include.se = TRUE,            
#     include.t = TRUE,           
#     include.pvalues = TRUE, 
#     include.dof = TRUE)

# icc_value <- icc(m1)



tab_model(m1, 
        file = "tables/m1_summary.html",
        show.re.var = TRUE,
        show.se = TRUE,        
        show.stat = TRUE,
        show.df = FALSE,
        show.p = TRUE,
        show.r2=FALSE
        )


anova_table = xtable(anova(m1))
print(anova_table, type = "latex")

ranova_table = xtable(ranova(m1))
print(ranova_table, type = "latex")




############## Linear model ##############
m2 = lm(YLD ~ GEN + ENV, data = data)
summary(m2)

tab_model(m2, 
        file = "tables/m2_summary.html",
        show.re.var = TRUE,
        show.se = TRUE,        
        show.stat = TRUE,
        show.df = FALSE,
        show.p = TRUE,
        show.r2=FALSE
        )


