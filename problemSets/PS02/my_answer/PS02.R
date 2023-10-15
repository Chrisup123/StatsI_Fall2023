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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################
#a
grandtotal<-14+6+7+7+7+1
row_total_1<-14+6+7
row_total_2<-7+7+1
col_total_1<-14+7
col_total_2<-6+7
col_total_3<-7+1
fe11<-row_total_1*col_total_1/grandtotal
fe12<-row_total_1*col_total_2/grandtotal
fe13<-row_total_1*col_total_3/grandtotal
fe21<-row_total_2*col_total_1/grandtotal
fe22<-row_total_2*col_total_2/grandtotal
fe23<-row_total_2*col_total_3/grandtotal
(14-fe11)^2/fe11
chi2<-((14-fe11)^2)/fe11+((6-fe12)^2)/fe12+((7-fe13)^2)/fe13+((7-fe21)^2)/fe21+((7-fe22)^2)/fe22+((1-fe23)^2)/fe23

#b
df<-(2-1)*(3-1)
p_value<-round(pchisq(chi2,df,lower.tail = FALSE),4)
#p_value (= 0.1502) is bigger than alpha (=0.1). So, we don't have enough 
#evidence to reject the null hypothesis that Officer are not likely to 
#solicit a bribe from drivers depending on their class.

#c
z11<-round((14-fe11)/sqrt((1-row_total_1/grandtotal)*(1-col_total_1/grandtotal)),4)
z12<-round((6-fe12)/sqrt((1-row_total_1/grandtotal)*(1-col_total_2/grandtotal)),4)
z13<-round((7-fe13)/sqrt((1-row_total_1/grandtotal)*(1-col_total_3/grandtotal)),4)
z21<-round((7-fe21)/sqrt((1-row_total_2/grandtotal)*(1-col_total_1/grandtotal)),4)
z22<-round((7-fe22)/sqrt((1-row_total_2/grandtotal)*(1-col_total_2/grandtotal)),4)
z23<-round((1-fe23)/sqrt((1-row_total_2/grandtotal)*(1-col_total_3/grandtotal)),4)

#d

#####################
# Problem 2
#####################

#2.
expenditure <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header=T)






