


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

plot(fit1, xlab="Survival Time in Days",
   ylab="% Surviving", yscale=100,
   main="Survival Distribution (Overall)")


#Post Hoc test ----


comp(ten(fit1))

res <- pairwise_survdiff(Surv(time=Data$TE, event=Data$Outcome) ~ Genetics,
     data = Data)

survminer::pairwise_survdiff(Surv(time=Data$TE, event=Data$Outcome) ~ Genetics,
                                    data = Data, p.adjust.method = "BH")
                                    
 library(emmeans)
 
 #have to change from integer to character
 
 str(Data)
 
 Data$Genetics <- as.character(Data$Genetics)
 
s2<-Surv(time=Data$TE, event=Data$Outcome, type="right")
P2<-survreg(s2~Treatment*Genetics, data=Data)
pa2<-anova(P2)
Prr<-emmeans(P2, ~Treatment*Genetics)
contrast(Prr, method="pairwise")





s2<-Surv(time=Data$TE, event=Data$Outcome, type="right")
P2<-survreg(s2~Treatment, data=Data)
pa2<-anova(P2)
Prr<-emmeans(P2, ~Treatment)
contrast(Prr, method="pairwise")



s2<-Surv(time=Data$TE, event=Data$Outcome, type="right")
P2<-survreg(s2~Genetics, data=Data)
pa2<-anova(P2)
Prr<-emmeans(P2, ~Genetics)
contrast(Prr, method="pairwise")


#getting error: Error in model.frame.default(formula = survival_data ~ Genetics, data = data[.subset,  : 
  variable lengths differ (found for 'Genetics')

> length(Surv(time = Data$TE, event = Data$Outcome))
[1] 432
> length(Data$Genetics)
[1] 432



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

#Look at Genetic effects

Data$Genetics <- as.character(Data$Genetics)

fit.coxph <- coxph(Surv(TE, Outcome) ~ Genetics, data = Data)

summary(fit.coxph)

# Create forest plot
library(survminer)

plot <- ggforest(
  fit.coxph,
  data = Data,
  main = "Hazard ratio",
  fontsize = 1,
  noDigits = 2
)

# Adjust font size
plot <- plot + theme(text = element_text(size = 14))

# Print the plot
print(plot)


#hazard ratio ---- 

fit.coxph =coxph(Surv(TE,Outcome)~Treatment,data=Data)

summary(fit.coxph)

plot <- ggforest(fit.coxph, data=Data, main = "Hazard ratio",
fontsize = 1,
noDigits = 2
)

plot <- plot + theme(text = element_text(size = 14))

plot


