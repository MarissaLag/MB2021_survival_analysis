


Data=read.table("PB2023_spat_challenge.txt",header=TRUE)



#Packages ----

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

  
#WD ---- 

setwd("/Users/"/Users/maris/OneDrive/Documents/USRA2021/mb2021")

getwd()

list.files(path=".")

#Data ----
 
Data=read.table("PB2023_spat_challenge.txt",header=TRUE)

head(Data)


#Surv curve ----

surv_object = Surv(time=Data$TE, event=Data$Outcome)

surv_object

fit1 = survfit(surv_object~Treatment, data=Data)

summary(fit1)

#risk.table = TRUE, tables.theme = theme_cleantable()   # to add risk table
#size = adjust line thickness

ggsurvplot(fit1, 
data=Data, 
pval = TRUE,
legend.title = "Treatment",
legend = "bottom",
legend.labs = c("Control", "Killed-Probiotics", "Probiotics"),
font.legend =c(12,"plain","black"),
break.time.by = 2,
pval.size = 7,
ggtheme = theme_classic(),
font.main = c(16, "bold"),
font.x = 14,
font.y = 14,
font.tickslab = 12,
size = 1.5,
xlab = "Time (Days)"
)

#plot cumulative hazard estimates - different from Kaplein-Meier, use "fun" argument.

ggsurvplot(fit1, 
data=Data,
fun = "cumhaz", 
conf.int = TRUE,
pval = TRUE,
legend.title = "Treatment",
legend = "bottom",
legend.labs = c("Control", "Killed-Probiotics", "Probiotics"),
font.legend =c(12,"plain","black"),
break.time.by = 2,
pval.size = 7,
ggtheme = theme_classic(),
font.main = c(16, "bold"),
font.x = 14,
font.y = 14,
font.tickslab = 12,
size = 1.5,
xlab = "Time (Days)"
)

#Facet_grid surv curve ----  


fit3 <- survfit( surv_object ~Treatment + Genetics,
                data = Data )
                
ggsurv <- ggsurvplot(fit3, 
data = Data, 
conf.int = TRUE,
risk.table = TRUE, 
risk.table.col="strata",
ggtheme = theme_classic(),
break.time.by = 2,
font.x = 14,
font.y = 14,
font.tickslab = 12,
xlab = "Time (Days)",
legend = "none",
surv.col = "Treatment"
)

curv_facet <- ggsurv$plot + facet_grid(Treatment ~ Genetics)

curv_facet




#hazard ratio ----

fit.coxph =coxph(Surv(TE,Outcome)~Treatment,data=Data)

summary(fit.coxph)

plot <- ggforest(fit.coxph, data=Data, main = "Hazard ratio",
fontsize = 1,
noDigits = 2
)

plot <- plot + theme(text = element_text(size = 14))

plot


  
