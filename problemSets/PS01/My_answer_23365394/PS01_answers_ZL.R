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

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#1.
n <-length(y)
sample_mean<-mean(y)
sample_sd<-sd(y)
z90<-qnorm((1-.9)/2,25,24,lower.tail = FALSE)
z90<-qnorm((1-.9)/2,lower.tail = FALSE)
lower90<-sample_mean-(z90*(sample_sd/sqrt(n)))
upper_90<-sample_mean+(z90*(sample_sd/sqrt(n)))
confident90<-c(lower90,upper_90)
confident90
#2.
#µ=100
#H0:µ0>µ
#H1:µ0<µ

df<-n-1
t<-(sample_mean-100)/(sample_sd/sqrt(n))
p_value<-2*pt(abs(t),df,lower.tail = FALSE)
p_value
#p_value= 0.5569233 >α = 0.05,we can not reject the null hypothesis, i.e. the average student IQ in her schoolis higher than the average IQ score (100)
#####################
# Problem 2
#####################
#1.
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

YX123<-data.frame(expenditure$X1, expenditure$Y,expenditure$X2, expenditure$X3)
pdf("plot_YX123.pdf")
plot(YX123)
dev.off()

#2.
# create scatterplot of region and Y
pdf("plot_regionandY.pdf")
plot(expenditure$Region, expenditure$Y)
dev.off()

#3.
# create scatterplot of X1 and Y and region
pdf("plot_X1andYandregion.pdf")
plot(expenditure$Region, expenditure$Y,col=expenditure$Region)
dev.off()







