
Cox_larval_vibrio <- read.delim("~/Documents/MSc/larval vibrio challenge/Cox_larval_vibrio.txt")


#Data=read.table("PB2023_spat_challenge.txt",header=TRUE)

#Data=read.table("mb2021_lowdose3_NEW_family_labels.txt",header=TRUE)

#Data=read.table("highdose_newnew2T17.txt",header=TRUE)


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
 
# Data=read.table("PB2023_spat_challenge.txt",header=TRUE)
# 
# Data=spat.challenge_mb2021_lowdose_correctfamilies_2024
# 
# Data=mb2021_lowdose_NEW_family_labels

Data = PB2024_2_spat_challenge_R

Data = PB2023_spat_challenge

head(Data)

#in mb2021 data may want to remove T9 - had no deaths in low treatment but died very quick in high dose treatment
#worried that T9 did not receive vibrio...T9 is also a major outlier in LS treatment. Lets try removing the data.

# Data_T9_removed <- Data %>% 
#   filter(!Tank == "9")
# 
# View(Data_T9_removed)
# 
# #Also going to try removed Family 1 altogether and see if Treatment trend remains. 
# 
# Data_T9_removed <- Data %>% 
#   filter(!Family == "1")
# 
# View(Data_T9_removed)

#MU42022 - remove family 4 

Data <- Data %>% 
  filter(!Genetics == "4")

Data <- Data %>% 
  filter(!Larval_Treatment %in% c("PB_Dil_2", "PB_Dil_3", "PB_Dil_4"))

Data <- Data %>% 
  filter(!Tank %in% c("23", "24"))


#Also replace Treatment numbering with Treatment names
#where (1=Control, 2=LS, and 2=HS)
#Can keep numerical format if you'd like, but make sure it is a character (not integer) for hazard ratio test. 

# Data$Treatment[Data$Treatment == 1] <- "Control"
# Data$Treatment[Data$Treatment == 2] <- "Low salinity"
# Data$Treatment[Data$Treatment == 3] <- "High salinity"
# 
# Data_T9_removed$Treatment[Data_T9_removed$Treatment == 1] <- "Control"
# Data_T9_removed$Treatment[Data_T9_removed$Treatment == 2] <- "Low salinity"
# Data_T9_removed$Treatment[Data_T9_removed$Treatment == 3] <- "High salinity"


#MU42022 renaming
Data$treatment[Data$treatment == "PH"] <- "Probiotics + HT"
Data$treatment[Data$treatment == "Probiotic"] <- "Probiotics"
Data$treatment[Data$treatment == "Heat"] <- "High temperature"
Data

#Surv curve ----

surv_object = Surv(time=Data$TE, event=Data$Outcome)

#surv_object = Surv(time=Data$TD, event=Data$Binary)

#surv_object = Surv(time=Data_T9_removed$Time.elapsed, event=Data_T9_removed$Outcome)

surv_object

fit1 = survfit(surv_object~Treatment, data=Data)

#fit1 = survfit(surv_object~Treatment, data=Data_T9_removed)

summary(fit1)

plot(fit1, xlab="Survival Time in Days", color = "Treatment",
   ylab="% Surviving", yscale=100,
   main="Survival Distribution (Overall)")

#Caution with ggsurvplot - axis titles can be mismatched, will
#follow alphabetical order

ggsurvplot(fit1, data=Data, pval = TRUE, legend = "bottom", legend.title="Treatment", font.legend =c(9,"plain","black"))
           #legend.labs=c("Control","Low_salinity","High_salinity"))

color_palette <- c("grey", "#FF7F00", "#4DAF4A","#377EB8") # Example color palette

# ggsurvplot with the specified color palette
# ggsurvplot(fit1, data = Data, pval = TRUE, legend = "bottom", legend.title = "Treatment",
#            font.legend = c(12, "plain", "black"), legend.labs = c("Control", "High temperature (HT)", "Probiotics + HT", "Probiotics"),
#            palette = color_palette, ylim = c(0.4, 1),
#            conf.int = FALSE,
#            lwd = 1.5,
#            linetype = "strata")

ggsurvplot(fit1, data = Data, pval = TRUE, legend = "bottom", legend.title = "Treatment",
           font.legend = c(12, "plain", "black"),
           palette = color_palette, ylim = c(0.2, 1),
           conf.int = FALSE,
           lwd = 1.5)

ggsurvplot(fit1, data = Data, pval = TRUE, legend = "bottom", legend.title = "Treatment",
           font.legend = c(12, "plain", "black"), 
           palette = color_palette, ylim = c(0, 1),
           conf.int = FALSE,
           lwd = 1.5,
           linetype = "strata")


#Log rank test ----

#Chi-Squared test
survdiff(Surv(time = TD, 
              event = Binary == "1") ~ Treatment + Family, 
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

Data$Genetics <- as.factor(Data$Genetics)
Data$Treatment <- as.factor(Data$Treatment)

res <- pairwise_survdiff(Surv(time=Data$TE, event=Data$Outcome) ~ Genetics,
                         data = Data)

survminer::pairwise_survdiff(Surv(time=Data$TE, event=Data$Outcome) ~ Genetics,
                             data = Data, p.adjust.method = "BH")

#getting error: Error in model.frame.default(formula = survival_data ~ Genetics, data = data[.subset,  : 
#variable lengths differ (found for 'Genetics')

length(Surv(time = Data$TE, event = Data$Outcome))
[1] 432
length(Data$Genetics)
[1] 432



  #Risk tables ----
#risk.table = TRUE, tables.theme = theme_cleantable()   # to add risk table
#size = adjust line thickness

color_palette <- c("grey", "#FF7F00", "#4DAF4A")

ggsurvplot(fit1, 
data=Data, 
pval = TRUE,
legend.title = "Treatment",
conf.int = TRUE,
legend = "bottom",
legend.labs = c("Control", "Killed-Probiotics", "Probiotics"),
font.legend =c(16,"plain","black"),
break.time.by = 2,
pval.size = 7,
ggtheme = theme_classic(),
font.main = c(16, "bold"),
font.x = 15,
font.y = 15,
font.tickslab = 14,
size = 1.5,
xlab = "Time (Days)",
palette = color_palette
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
xlab = "Time (Days)",
palette = color_palette
)

#Facet_grid surv curve ----  
surv_object = Surv(time=Data$TE, event=Data$Outcome)
fit3 <- survfit( surv_object ~ Treatment + Genetics,
                 data = Data )
ggsurvplot(
  fit3, 
  data = Data, 
  conf.int = TRUE,
  # risk.table.col = "strata",
  ggtheme = theme_classic() +
    theme(strip.text = element_text(size = 14)), # Adjust font size and style
  break.time.by = 2,
  font.x = 14,
  font.y = 14,
  font.tickslab = 12,
  xlab = "Time (Days)",
  legend = "bottom",
  surv.col = "Treatment",
  legend.labs = c("Control", "Killed-Probiotics", "Probiotics"), 
  facet.by = "Genetics",
  palette = color_palette
)

ggs <- ggsurvplot(
  fit3, 
  data = Data, 
  conf.int = FALSE,
  pval = TRUE,
  ggtheme = theme_classic(),
  break.time.by = 2,
  font.x = 14,
  font.y = 14,
  font.tickslab = 12,
  xlab = "Time (Days)",
  legend = "bottom",
  surv.col = "Treatment",
  legend.labs = c("Control - F1", "Killed-Probiotics - F1", "Probiotics - F1","Control - F2", "Killed-Probiotics - F2", "Probiotics - F2", "Control - F4", "Killed-Probiotics - F4", "Probiotics - F4"), 
)

plot <- ggs + facet_grid(~Genetics)



#Hazard ratio ----

fit.coxph =coxph(Surv(TE,Outcome)~Treatment*Genetics,data=Data)
#fit.coxph =coxph(Surv(Time.elapsed,Outcome)~Treatment,data=Data_T9_removed)

summary(fit.coxph)

plot <- ggforest(fit.coxph, data=Data, main = "Hazard ratio",
fontsize = 1.3,
noDigits = 2
)

plot <- plot + theme(text = element_text(size = 15
                                        ))

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


#Contrasts (attempts) ----


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


#Parametric analysis (attempts) ----
#trying parametric survival analysis
#convert all variables to factors


install.packages("flexsurv")
install.packages("SurvRegCensCov")
install.packages("gtsummary")
library(dplyr)
library(gtsummary)
library(broom)
library(flexsurv)
library(SurvRegCensCov)

str(Data)

d1<- Data %>% 
  dplyr::select(animal, Treatment, Genetics, TE, Outcome) %>%
  mutate_if(is.character, as.factor)
glimpse(d1)

# d1$Treatment <- recode(d1$Treatment, "Control" = "1", "Probiotics" = "2", "Killed-Probiotics" = "3")


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

exp.mod.aft <- survreg(Surv(TE, Outcome) ~ Genetics*Treatment, 
                       data = d1, dist = 'exponential')
summary(exp.mod.aft)

#estimating a Weibull parametric survival model which will return a accelerated failure metric:
#Distribution: The Weibull distribution generalizes the exponential distribution by allowing the hazard rate to change over time. It can model:
# Increasing hazard rates (aging-related risks).
# Decreasing hazard rates (burn-in periods or early failures).

wei.mod.aft <- survreg(Surv(TE, Outcome) ~ Treatment*Genetics, 
                       data = d1, dist = 'weibull')
summary(wei.mod.aft)
AIC(wei.mod.aft)
# Extracting residuals
residuals_wei <- residuals(wei.mod.aft)

# Plot residuals
plot(residuals_wei, main = "Residuals of Weibull Model", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")

qqnorm(residuals_wei)
qqline(residuals_wei, col = "red")

#Export summary as table
install.packages(c("broom", "kableExtra"))  # or use "gt"
library(broom)
library(kableExtra)  # for kableExtra tables

# Create a tidy summary of the model
tidy_summary <- tidy(wei.mod.aft)
# Create a formatted table
summary_table <- tidy_summary %>%
  kable("html", caption = "Weibull Model Summary") %>%
  kable_styling(full_width = F, position = "left")

