


Data=read.table("Data.txt",header=TRUE)
Pedigree=read.table("Pedigree.txt",header=TRUE)




********Cox Analysis*******
install.packages("survival")

install.packages("survminer")

install.packages("dplyr")

install.packages("ggplot2")

install.packages("ggpubr")

***

library(survival)

library(survminer)

library(dplyr)

library(ggplot2)

library(ggpubr)

*******

setwd("/Users/tim/Desktop/Marissa_MB_Challenge")

getwd()

list.files(path=".")

****

Data=read.table("Data_low_dose.txt",header=TRUE)

head(Data)

surv_object = Surv(time=Data$TD, event=Data$Binary)

surv_object

fit1 = survfit(surv_object~treatment, data=Data)

summary(fit1)

ggsurvplot(fit1, data=Data, pval = TRUE, legend = "bottom", legend.title="Treatment", font.legend =c(9,"plain","black"), legend.labs=c("Control","Low_salinity","High_salinity"))


*****fit a cox proportional hazards model for size and family*****

****note family as text in data file****

fit.coxph =coxph(Surv(TD,Binary)~treatment,data=Data)

summary(fit.coxph)

ggforest(fit.coxph, data=Data)

