


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


#contracts ----


#PH assumption - relative hazard remains constant over time with different predictor or covariate levels.
#hazards vary because the susceptibility of a disease varies between patients
#PH assumption implies the HR measuring the effect of any predictor is constant over time.

zph <- cox.zph(interact, transform = 'km')
zph

plot(zph, var = "Treatment")
plot(zph, var = "Genetics")

#if violates PH assumption (p<0.05) - In the case of serious violation of proportionality of hazard, we can remedy using
#stratified cox regression or
#extended cox regression using time-varying dependent variable or
#parametric survival analysis


