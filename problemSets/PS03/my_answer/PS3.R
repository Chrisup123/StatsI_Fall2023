#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

# Question 1 --------------
# (1) 

#run the regression
lm_vote_diff=lm(inc.sub$voteshare~inc.sub$difflog)
summary(lm_vote_diff)
#According to the regression,we can see the based on the t value of Intercept and difflog,
#the doesn't equal to 0 significantly. And according to the Muultiple R-squared=0.3673
#we can say difflog only can exaplain around 37% change in voteshare.

# (2) 
#name the picture
pdf("lm_vote_diff.pdf")

# Create a scatterplot
plot(inc.sub$difflog,inc.sub$voteshare, ch = 16, col = "blue", main = "Scatterplot with Regression Line", xlab = "difflogp", ylab = "voteshare")

# Add regression line to the plot
abline(lm_vote_diff, col = "red")

# Add a legend
legend("topright", legend = "Regression Line", col = "red", lty = 1)

#save the picture
dev.off()

# (3) 
#Save the residuals as a separate object
residuals_vote_diff <- residuals(lm_vote_diff)
summary(residuals_vote_diff)

# (4) 
#the equationb: voteshare = 0.58+0.04*difflog,
#where 1)B0=0.58, which means when difflog equals to 0, the 
#.       mean of voteshare equals to 0.58
#      2)B1=0.04,which means when difflog increase by one unite, 
#.      the mean of voteshare would increase by 0.04

# ----------------------------------------------------------------------
# Question  2 --------------
# (1) 
#run the regression
lm_pres_diff=lm(inc.sub$presvote~inc.sub$difflog)
summary(lm_pres_diff)
#According to the regression,we can see the based on the t value of Intercept and difflog,
#they don't equal to 0 significantly. And according to the Muultiple R-squared=0.08795
#we can say difflog only can exaplain around 9% change in presvote.

# (2) 
#name the picture
pdf("lm_pres_diff.pdf")

# Create a scatterplot
plot(inc.sub$difflog,inc.sub$presvote, ch = 16, col = "blue", main = "Scatterplot with Regression Line", xlab = "difflogp", ylab = "presvote")

# Add regression line to the plot
abline(lm_pres_diff, col = "red")

# Add a legend
legend("topright", legend = "Regression Line", col = "red", lty = 1)

#save the picture
dev.off()
# (3) 
#Save the residuals as a separate object
residuals_pres_diff <- residuals(lm_pres_diff)
summary(residuals_pres_diff)

# (4) 
#the equationb: presvote = 0.58+0.04*difflog,
#where 1)B0=0.58, which means when difflog equals to 0, the 
#.       mean of presvote equals to 0.58
#      2)B1=0.04,which means when difflog increase by one unite, 
#.      the mean of presvote would increase by 0.04

# ----------------------------------------------------------------------
# Question  3 --------------
# (1) 
#run the regression
lm_vote_pres=lm(inc.sub$voteshare~inc.sub$presvote)
summary(lm_vote_pres)
#According to the regression,we can see the based on the t value of Intercept and difflog,
#they don't equal to 0 significantly. And according to the Muultiple R-squared=0.2058
#we can say presvote only can exaplain around 20% change in voteshare.
# (2) 
#name the picture
pdf("lm_vote_pres.pdf")

# Create a scatterplot
plot(inc.sub$presvote,inc.sub$voteshare, ch = 16, col = "blue", main = "Scatterplot with Regression Line", xlab = "presvote", ylab = "voteshare")

# Add regression line to the plot
abline(lm_vote_pres, col = "red")

# Add a legend
legend("topright", legend = "Regression Line", col = "red", lty = 1)

#save the picture
dev.off()
# (3) 
#the equationb: voteshare = 0.44+0.38*presvote,
#where 1)B0=0.44, which means when presvote equals to 0, the 
#.       mean of voteshare equals to 0.44
#      2)B1=0.38,which means when presvote increase by one unite, 
#.      the mean of voteshare would increase by 0.38

# ----------------------------------------------------------------------
# Question  4 --------------
# (1) 
#run the regression
lm_residuals=lm(residuals_vote_diff~residuals_pres_diff)
summary(lm_residuals)
#According to the regression,we can see the based on the t value of residual_pres_diff
#they don't equal to 0 significantly. However, 95% CI of Intercept include 0, so
# it is plausible that residuals_vote_cliff equals to 0 when residual_pres_diff euquals to 0
#And according to the Muultiple R-squared=0.13, we can say residual_pres_diff 
#only can exaplain around 13% change in residuals_vote_cliff.

# (2) 
pdf("lm_residuals.pdf")

# Create a scatterplot
plot(residuals_pres_diff,residuals_vote_diff, ch = 16, col = "blue", main = "Scatterplot with Regression Line", xlab = "residuals_pres_diff", ylab = "residuals_vote_diff")

# Add regression line to the plot
abline(lm_residuals, col = "red")

# Add a legend
legend("topright", legend = "Regression Line", col = "red", lty = 1)

#save the picture
dev.off()

# (3) 
#the equationb: residuals_vote_diff = (-1.942e-18)+(2.569e-01)*residuals_pres_diff,
#where 1)B0=(-1.942e-18), which means when residuals_pres_diff equals to 0, the 
#.       mean of residuals_vote_diff equals to  (-1.942e-18)
#      2)B1=(2.569e-01),which means when residual_vote_diff increase by one unite, 
#.      the mean of residuals_vote_diff would increase by (2.569e-01)

# (5) Question  5--------------
# (1) 
#run the regression
lm_vote_diff_pres=lm(inc.sub$voteshare~inc.sub$difflog+inc.sub$presvote)
summary(lm_vote_diff_pres)
#According to the regression,we can see the based on the t value of Intercept difflog 
#and presvote,they don't equal to 0 significantly. And according to the Muultiple 
#R-squared=0.4496,we can say difflog and presvote only can exaplain around 45% change in presvote.

# (2) 
#the equationb: voteshare = 0.45+0.04*difflog+0.26*presvote,
#where 1)B0=0.45, which means when difflog and presvote equals to 0, the 
#.       mean of residuals_vote_diff equals to 0.45
#      2)B1=0.04, which means when presvote keep the same and difflog increase
#.       by one unite, the mean of rvoteshare would increase by 0.04
#.     3)B2=0.26, which means when difflog keep the same and presvote increase 
#.       by one unite, the mean of voteshare would increase by 0.26

# (3) 
#the residuals and residual standard error in the output of Question 5 is 
#identical to that of Question 4. But the resaon is unknown.


