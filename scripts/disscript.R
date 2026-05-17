library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(psych)
library(sjlabelled) 
library(sjmisc)
library(e1071)
library(psycho)
library(knitr)
library(scales)
library(performance)
library(car)
library(plm)
library(sjPlot)

#CLEANING AND PREPARING DATA
names(CRE_and_contact_hypothesis)

#changing format to long
long_data <- CRE_and_contact_hypothesis %>%
  pivot_longer(
    cols = starts_with(c("w_", "bc_", "i_")),
    names_to = c("race_face", "type_num"),
    names_sep = "_",
    values_to = "recognition"
  ) %>%
  mutate(
    racestim = case_when(
      str_detect(race_face, "^w") ~ "White",
      str_detect(race_face, "^bc") ~ "Black Caribbean",
      str_detect(race_face, "^i") ~ "Indian",
      TRUE ~ NA_character_
    ),
    facetype = if_else(str_detect(type_num, "target"), "Target", "Distractor")
  ) %>%
  select(-race_face, -type_num)

#converting 'recognition' to numeric values
long_data$recognition<-ifelse(long_data$recognition=="Yes",1,0)
is.numeric(long_data$recognition)
table(long_data$recognition)

#adding columns for 'hit', 'false alarm', 'miss', and 'correct rejection'
long_data <- long_data %>%
  mutate(
    hit = if_else(facetype == "Target" & recognition == 1, 1, 0),
    miss = if_else(facetype == "Target" & recognition == 0, 1, 0),
    falsealarm = if_else(facetype == "Distractor" & recognition == 1, 1, 0),
    corrreject = if_else(facetype == "Distractor" & recognition == 0, 1, 0)
  )

#re-naming age
names(long_data)[names(long_data) == "Which age group do you fit into?"] <- "age"

#calculating d'
dprime_data <- long_data %>%
  group_by(ID, racestim, age) %>%
  summarize(
    hits = sum(hit),
    false_alarms = sum(falsealarm),
    total_targets = sum(facetype == "Target"),  #number of targets
    total_distractors = sum(facetype == "Distractor"),  #number of distractors
    HR = (hits + 0.5) / (total_targets + 1),  #hit rate with correction
    FAR = (false_alarms + 0.5) / (total_distractors + 1),  #fa rate with correction
    .groups = 'drop'
  ) %>%
  mutate(
    zHR = qnorm(HR),
    zFAR = qnorm(FAR),
    d_prime = zHR - zFAR
  )

print(dprime_data)

#UNIVARIATE ANALYSIS
#dependent variable 'd_prime'- recognition accuracy
summary(dprime_data$d_prime)
summary(dprime_data$hits)
summary(dprime_data$false_alarms)

describe(dprime_data$d_prime)
describe(dprime_data$hits)
describe(dprime_data$false_alarms)

IQR(dprime_data$d_prime)
IQR(dprime_data$hits)
IQR(dprime_data$false_alarms)

ggplot(data=dprime_data, aes(y=d_prime, x="")) + geom_boxplot()+
  ylab("Recognition accuracy")
ggplot(data=dprime_data, aes(x=d_prime)) + geom_density() +
  xlab("Recognition accuracy")
ggplot(data=dprime_data, aes(x=hits)) + geom_density() +
  xlab("Hits")
ggplot(data=dprime_data, aes(x=false_alarms)) + geom_density() +
  xlab("False alarms")
 

meanacc <- mean(dprime_data$d_prime, na.rm = T)
medianacc <- median(dprime_data$d_prime, na.rm = T)
ggplot(data=dprime_data, aes(x=d_prime)) + geom_density() +
  geom_vline(xintercept=meanacc, colour="blue") + # vertical line for the mean in blue
  geom_vline(xintercept=medianacc, colour="red") + # vertical line for the median in red
  xlab("Recognition accuracy")+
  ggtitle("Figure 1: Distribution of recognition accuracy")

#independent variables 'racestim' and 'age'
dprime_data |> 
  count(racestim) |>
  mutate(perc = percent(n / sum(n), 0.01)) |>
  kable()

frq(dprime_data$racestim, out = "v", show.na = FALSE,
    title = "Table 1: Distribution of race of stimuli",
    file="table_racestim.doc")

ggplot(data = dprime_data, aes(x = racestim)) +
  geom_bar(stat = "count")+
  xlab("Race of stimuli") + 
  ylab("Number of stimuli") + 
  ggtitle("Stimuli per racial group")+
  scale_x_discrete(labels=c("Black Caribbean","Indian","White"))

dprime_data |> 
  count(age) |>
  mutate(perc = percent(n / sum(n), 0.01)) |>
  kable()
frq(dprime_data$age, out = "v", show.na = FALSE,
    title = "Table 2: Distribution of variable 'age'",
    file="disstables.doc")
ggplot(data = dprime_data, aes(x = age)) +
  geom_bar(stat = "count")+
  ggtitle("Age of participants")

#BIVARIATE ANALYSIS - testing 1st hypothesis
by(dprime_data$d_prime,dprime_data$racestim,describe)
by(dprime_data$hits,dprime_data$racestim,describe)
by(dprime_data$false_alarms,dprime_data$racestim,describe)

anova_result <- aov(d_prime ~ racestim, data = dprime_data)
summary(anova_result)
TukeyHSD(aov(anova_result))
anova_result1 <- aov(false_alarms ~ racestim, data = dprime_data)
summary(anova_result1)
TukeyHSD(aov(anova_result1))

capture.output(summary(anova_result),file="anova.doc") 

ggplot(data=dprime_data, aes(y=d_prime, x=racestim)) + geom_boxplot()+
  ggtitle("Recognition accuracy by racial group")+
  xlab("Race of stimuli")+
  ylab("Recognition accuracy")+
  scale_x_discrete(labels=c("Black Caribbean","Indian","White"))

ggplot(data=dprime_data, aes(x=d_prime, colour=racestim)) + geom_density()+
  ggtitle("Figure 4: Recognition accuracy by racial group")+
  xlab("Recognition accuracy")+
  theme(legend.title=element_blank())

ggplot(data=dprime_data, aes(x=hits, colour=racestim)) + geom_density()+
  ggtitle("Figure 5: Hits by racial group")+
  xlab("Recognition accuracy")+
  theme(legend.title=element_blank())

ggplot(data=dprime_data, aes(x=false_alarms, colour=racestim)) + geom_density()+
  ggtitle("Figure 6: False alarms by racial group")+
  xlab("Recognition accuracy")+
  theme(legend.title=element_blank())

sjt.xtab(dprime_data$d_prime, dprime_data$racestim, show.col.prc = TRUE, 
         title="Table 3: Recognition accuracy across the three racial groups: White, Indian, and Black Caribbean", 
         file = "recognrace_table.doc")

sjt.xtab(dprime_data$d_prime, dprime_data$age, show.col.prc = TRUE, 
         title="Table 4: Recognition accuracy across age groups", 
         file = "recognage_table.doc")

#fixed effects model - recognition accuracy and race of stimuli
FE = plm(d_prime ~ racestim, data=dprime_data, model = "within")
summary(FE)

plot_model(FE, title = "Regression coefficients for FE predictors",
           sort.est=FALSE)
tab_model(FE, 
          dv.labels = c("Recognition accuracy (FE)"),
          pred.labels=c("Indian","White"),
          show.se=TRUE, show.r2 = TRUE, show.fstat = TRUE, show.aic = TRUE,
          file="FEmodel.doc")

#recognition accuracy and age
by(dprime_data$d_prime,dprime_data$age, describe)
anova_result1 <- aov(d_prime ~ age, data = dprime_data)
summary(anova_result1)

#model with recognition accuracy, race, and age
model1 <- lm(d_prime ~ racestim+age, data=dprime_data)
summary(model1) #not significant

tab_model(model1, 
          dv.labels = c("Model 2"),
          pred.labels=c("Intercept","Indian","White", "25-34","35-44","45-54","55-64"),
          show.se=TRUE, show.r2 = TRUE, show.fstat = TRUE, show.aic = TRUE,
          file="agemodel.doc")
