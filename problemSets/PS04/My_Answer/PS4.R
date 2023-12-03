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

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Load libraries
install.packages(car) 
library(car)

#laod the dataset
data(Prestige) 
help(Prestige)

#create Dataframe
df<-Prestige
View(df)

# Question 1 --------------
#(a)
#clean data, deleting imcomplete observation
df_na <- df[complete.cases(df[,c("income", "prestige", "type")]),] 
# Create a new variable professional by recoding the variable type
# professionals are coded as 1, and blue 
# and white collar workers are coded as 0
df_na$professional <- ifelse(df_na$type == "prof", 1, 0)

#(b)
pre_inc_pro<-lm(prestige~income+professional+income:professional,data=df_na)
summary(pre_inc_pro)
#Y=21.1423+0.0032*X1+37.7812*X2-0.0023*X1*X2
#Y:A measure of the prestige of the occupation, often based on social 
#  status or perceived importance.
#
#X1:The average income for individuals in a given occupation.
#
#X2:The type of occupation. professionals are coded as 1, and blue 
#   and white collar workers are coded as 0

#(c)
#When professional is 1
#Y=21.1423+0.0032*X1+37.7812*1-0.0023*X1*1
#that is, Y=58.9244+0.0009*X1

#When professional is 0
#Y=21.1423+0.0032*X1

#(d)
#The coefficient for income is 0.0032. It means when occupation 
#is professionals each unit increase in income would lead to 
#0.0032 average increase in the prestige in general.

#(e)
# The coefficient for professional is 37.7812. It means, compared to blue 
# and white collar workers, the prestige of occupation would increase by 37.7812
#in general when the occupation is professionals.

#(f)
#When professional=1, in general the effect of a $1,000 
#increase in income on prestige score would be:
x1<-runif(1, min = 0)
Y<-(58.9244+0.0009*(1000+x1))-(58.9244+0.0009*x1)#Y=0.9

#(g)
#when her income is $6,000, in general the effect of 
#changing occupations of somebody from non-professional to 
#professional would be:
Y<-(21.1423+0.0032*6000)-(58.9244+0.0009*6000)
#Y=-23.9821

# ----------------------------------------------------------------------
# Question  2 --------------

#（a）
# H0 : B_assigned= 0 vs. H1: B_assigned != 0
# get t_statistic and p_values
df_assigned<-131-2-1
TS_assigned <- ( 0.042 - 0 ) /0.016
p_value_assigned <- 2 * pt ( abs ( TS_assigned ) , df_assigned, lower.tail= F )
p_value_assigned
#According to calculation, the p_value is 0.00972, which is 
#lower than 0.05 so we make a conclusion that we can reject 
#the Null hypothesis and accept the alternative hypothesis 
#that the coefficient of the dummy variable in the question
#-- Precinct assigned lawn signs -- is is statistically 
#differentiable from zero.

#（b）
#H0 : B_adjacent = 0 vs. H1_adjacent : B!= 0
# get t_statistic and p_values
df_adjacent<-131-2-1
TS_adjacent <- ( 0.042 - 0 ) /0.013
p_value_adjacent <- 2 * pt ( abs ( TS_adjacent ) , df_adjacent , lower.tail= F )
p_value_adjacent
#According to calculation, the p_value is 0.001569, which 
#is lower than 0.05 so we make a conclusion that we can 
#reject the Null hypothesis and accept the alternative 
#hypothesis that the coefficient of the dummy variable in 
#the question-- Precinct adjecent to lawn signs -- is is 
#statistically differentiable from zero.

#（c）
#At precinct that is neither assigned lawns assign nor 
#adjacent to lawn signs, the estimated value of the 
#dependent variable--the mean of proportion of the 
#vote that went to McAuliffs opponent Ken Cuccinelli
#--would equals to the coefficient for the constant,which
#is 0.302

#（d）
#The R^2 of this regression is 0.094, which mean the 
#regression only explain 9.4% variance of the dependent 
#variable--the mean of proportion of the vote that 
#went to McAuliffs opponent Ken Cuccinelli. We can 
#make a conclusion from this R^2 that the yard sign 
#only can explain a very smart portion of the variance 
#of the dependent variable, so there must be others 
#factors play a more important role on the variance 
#of dependent variable.






