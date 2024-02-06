


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

library(emmeans)


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

#Log rank test ----

#Chi-Squared test
survdiff(Surv(time = TE, 
              event = Outcome == "1") ~ Treatment + Genetics, 
         data = Data,
         rho = 0)
#pairwise
survdiff <- pairwise_survdiff(Surv(time = TE, 
              event = Outcome == "1") ~ Treatment + Genetics, 
         data = Data,
         rho = 0)

write.table(survdiff, file = "survdiff_table.txt", sep = "\t", row.names = FALSE)

# Save as a comma-separated values (CSV) file using write.csv
write.csv(data_matrix, file = "data_table.csv", row.names = FALSE)


survdiff_table <- tbl_summary(
  survdiff,
  by = c("strata1", "strata2"),
  missing = "no"
)

# Print the table
survdiff_table
write.csv(survdiff, file = "Survdiff_table.csv", row.names = FALSE)


#Post Hoc test ----

 
 #have to change from integer to character
 
 str(Data)
 
 Data$Genetics <- as.character(Data$Genetics)
 
 #run post-hoc
 #source: https://stackoverflow.com/questions/77752878/using-post-hoc-testing-survreg-with-emmeans-in-r-when-certain-experimental-t 
 
s2<-Surv(time=Data$TE, event=Data$Outcome, type="right")
P2<-survreg(s2~Treatment*Genetics, data=Data)
pa2<-anova(P2)
Prr<-emmeans(P2, ~Treatment*Genetics)
Prr2 <- contrast(Prr, method="pairwise")

write.csv(Prr2, file = "Prr2_table.csv", row.names = FALSE)

Prr2_table <- read_csv("Prr2_table.csv")

#heatmap - data gas to be in correct format
#example format
data <- as.matrix(mtcars)
View(data)
heatmap(data)


Prr2_matrix <- as.matrix(Prr2_table)

View(Prr2_matrix)

row_names <- Prr2_matrix[, 1]

rownames(Prr2_matrix) <- row_names

#remove column

updated_matrix <- Prr2_matrix[, -c(1, 2, 3, 4)]

str(Prr2_matrix)

View(updated_matrix)

updated_matrix <- apply(updated_matrix, 2, as.numeric)

heatmap(updated_matrix, 
        Rowv = NA, 
        Colv = NA, 
        col = cm.colors(256),
        scale = "column",  # Use "row" or "none" for different scaling options
        main = "Heatmap Example",
        xlab = "Columns",
        ylab = "Rows")

#format as heatmap?




#other methods? code below doesn't work yet

comp(ten(fit1))

res <- pairwise_survdiff(Surv(time=Data$TE, event=Data$Outcome) ~ Genetics,
                         data = Data)

survminer::pairwise_survdiff(Surv(time=Data$TE, event=Data$Outcome) ~ Genetics,
                             data = Data, p.adjust.method = "BH")

#getting error: Error in model.frame.default(formula = survival_data ~ Genetics, data = data[.subset,  : 
  variable lengths differ (found for 'Genetics')

> length(Surv(time = Data$TE, event = Data$Outcome))
[1] 432
> length(Data$Genetics)
[1] 432



  #Risk tables ----
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


fit.coxph =coxph(Surv(TE,Outcome)~Treatment,data=Data)

summary(fit.coxph)

plot <- ggforest(fit.coxph, data=Data, main = "Hazard ratio",
fontsize = 1,
noDigits = 2
)

plot <- plot + theme(text = element_text(size = 14))

plot


#test for interactions

install.packages("gtsummary")
install.packages("gt")
install.packages("broom")
library(gtsummary)
library(gt)
library(broom)


interact <- 
  coxph(Surv(time = TE, 
             event = Outcome == '1') ~ Treatment + Genetics + Treatment:Genetics, 
        data = Data)

tbl_regression(interact, exponentiate = TRUE) %>%
  as_gt()

no_interact <- 
  coxph(Surv(time = TE, 
             event = Outcome == '1') ~ Treatment + Genetics, 
        data = Data)
tidy(no_interact, exponentiate = TRUE, conf.int = TRUE)


anova(interact,no_interact, test = 'Chisq')

#if p<0.05 you should include the interaction


#contrasts ----


#PH assumption - relative hazard remains constant over time with different predictor or covariate levels.
#hazards vary because the susceptibility of a disease varies between patients
#PH assumption implies the HR measuring the effect of any predictor is constant over time.

zph <- cox.zph(interact, transform = 'km')
zph

plot(zph, var = "Treatment")
plot(zph, var = "Genetics")

#if violates PH assumption (p<0.05) - In the case of serious violation of proportionality of hazard, we can remedy using
#stratified cox regression (i.e., exclude covariates that do not pass assumption) or
#extended cox regression using time-varying dependent variable or
#parametric survival analysis

#source for below: https://bookdown.org/drki_musa/dataanalysis/parametric-survival-analysis.html#advantages-of-parametric-survival-analysis-models


#Parametric analysis ----
#trying parametric survival analysis
#convert all variables to factors


install.packages("flexsurv")
install.packages("SurvRegCensCov")
library(dplyr)
library(gtsummary)
library(broom)
library(flexsurv)
library(SurvRegCensCov)

d1<- Data %>% 
  dplyr::select(animal, Treatment, Plate, Genetics, Tank, TE, Outcome) %>%
  mutate_if(is.character, as.factor)
glimpse(d1)

d1$Treatment <- recode(d1$Treatment, "Control" = "1", "Probiotics" = "2", "Killed-Probiotics" = "3")


#eda = exploratory data analysis

d1 %>% 
  mutate(status = recode(Outcome,"0" = "Censored", "1" = "Death")) %>% 
  tbl_summary(by = status,
              statistic = list(all_continuous() ~ "{mean} ({sd})", 
                               all_categorical() ~ "{n} ({p}%)"),
              type = list(where(is.logical) ~ "categorical"),
              label = list(Treatment ~ "Treatment", 
                           Genetics ~ "Family", 
                           TE ~ "Time of death"),
              missing_text = "Missing") %>% 
  modify_caption("**EDA**")  %>%
  modify_header(label ~ "**Variable**") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Survival Status**") %>%
  modify_footnote(all_stat_cols() ~ "Mean (SD) or Frequency (%)") %>%
  bold_labels() %>%
  as_gt()


#We can use the survreg() from survival package. However survreg() only perform estimation for AFT metric.

exp.mod.aft <- survreg(Surv(TE, Outcome) ~ Genetics + Treatment, 
                       data = d1, dist = 'exponential')
summary(exp.mod.aft)


#results:
Call:
  survreg(formula = Surv(TE, Outcome) ~ Treatment + Genetics, data = d1, 
          dist = "exponential")
Value Std. Error     z      p
(Intercept)                 2.4704     0.1546 15.98 <2e-16
TreatmentKilled-Probiotics  0.2654     0.1476  1.80 0.0721
TreatmentProbiotics         0.4540     0.1549  2.93 0.0034
Genetics                   -0.0157     0.0502 -0.31 0.7550

Scale fixed at 1 

Exponential distribution
Loglik(model)= -932.8   Loglik(intercept only)= -937.3
Chisq= 9.04 on 3 degrees of freedom, p= 0.029 
Number of Newton-Raphson Iterations: 4 
n= 432

#estimating a Weibull parametric survival model which will return a accelerated failure metric:

wei.mod.aft <- survreg(Surv(TE, Outcome) ~ Treatment + Genetics, 
                       data = d1, dist = 'weibull')
summary(wei.mod.aft)

survreg(formula = Surv(TE, Outcome) ~ Treatment + Genetics, data = d1, 
        dist = "weibull")
Value Std. Error      z       p
(Intercept)                 2.23723    0.03670  60.96 < 2e-16
TreatmentKilled-Probiotics  0.09302    0.03450   2.70   0.007
TreatmentProbiotics         0.15024    0.03631   4.14 3.5e-05
Genetics                   -0.00486    0.01173  -0.41   0.678
Log(scale)                 -1.46263    0.05488 -26.65 < 2e-16

Scale= 0.232 

Weibull distribution
Loglik(model)= -726.2   Loglik(intercept only)= -735.4
Chisq= 18.5 on 3 degrees of freedom, p= 0.00035 
Number of Newton-Raphson Iterations: 5 

ConvertWeibull(wei.mod.aft, conf.level = 0.95)
