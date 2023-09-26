


Data=read.table("PB2023_spat_challenge.txt",header=TRUE)





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

setwd("/Users/"/Users/maris/OneDrive/Documents/USRA2021/mb2021")

getwd()

list.files(path=".")

****

Data=read.table("Data_low_dose.txt",header=TRUE)

head(Data)

surv_object = Surv(time=Data$TE, event=Data$Outcome)

surv_object

fit1 = survfit(surv_object~Treatment, data=Data)

summary(fit1)

ggsurvplot(fit1, data=Data, pval = TRUE, legend = "bottom", legend.title="Treatment", font.legend =c(9,"plain","black")


fit.coxph =coxph(Surv(TE,Outcome)~Treatment,data=Data)

summary(fit.coxph)

plot <- ggforest(fit.coxph, data=Data)

plot <- plot + theme(text = element_text(size = 14))

plot

