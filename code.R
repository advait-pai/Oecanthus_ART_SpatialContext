"This script is used to analyse the data from the within-bush and across-bush
experiments to estimate the mating success of alternative reproductive tactics 
of male crickets in various spatio-acoustic scenarios.

Click Source in RStudio to run the entire script.

Manual Version Number: v1.6

Input: 15 .csv files containing data from experimental trials 

Output: A folder ('output') that includes 10 .csv files containing 95% bootstrapped
confidence intervals, the seven GLM result tables (1, 2, 3, 4, 5, 6, 7), and the results 
for the paired t-tests and proportion tests, and a folder containing plots"

#### Importing relevant libraries and setting working directory ####
# install.packages(c("tidyverse", "MASS", "EnvStats", "ggplot2", "glmmTMB", "lme4", "car", "emmeans")
library(tidyverse)
library(MASS)
library(EnvStats)
library(ggplot2)
library(glmmTMB)
library(lme4)
library(car)
library(emmeans)
library (patchwork)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to the folder the code is in


# Create folder to store output
folder_name <- "output"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
} 

# Create folder to store plots
folder_name <- "plots"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}

### Within-Bush Experiments ####

# Main data importing for within bush, virgin female experiments
data_virgin_female_same_bush <- read.csv('data_virgin_female_same_bush.csv')
# data_virgin_female_same_bush_without_repeating_males <- read.csv('data_summary_virgin_female_same_bush_wo_repeating_males.csv')
data_virgin_female_same_bush_baffler <- read.csv('data_virgin_female_same_bush_baffler.csv')

# Subset the data to include only a particular treatment 
data_single_male <- data_virgin_female_same_bush %>% 
  filter(treatment == 'single_male') # single male treatment
data_two_males_caller <- data_virgin_female_same_bush %>% 
  filter(treatment == 'two_males')  # two male treatment with callers
data_two_males_baffler <- data_virgin_female_same_bush_baffler # two male treatment with bafflers

# data_single_male_non_repeating_males <- data_virgin_female_same_bush_without_repeating_males%>% 
  # filter(treatment == 'single_male') # single male treatment involving naive males only 
# data_two_males_caller_non_repeating_males <- data_virgin_female_same_bush_without_repeating_males%>% 
  # filter(treatment == 'two_male') # two male male treatment involving naive males only 

## Calculating mating success (Within-Bush) ####

# Counting number of successful matings and total trials for each of the 6 treatments (Fig 3A, 3B)
mate_silent <- sum(data_single_male$mated == '1' & data_single_male$tactic == 'silent')
total_silent <- sum(data_single_male$tactic == 'silent')

mate_sat_with_cal <- sum(data_two_males_caller$mated == '1' & data_two_males_caller$tactic == 'silent')
total_sat_with_cal <- sum(data_two_males_caller$tactic == 'silent')

mate_sat_with_baf <- sum(data_two_males_baffler$mated == '1' & data_two_males_baffler$tactic == 'silent')
total_sat_with_baf <- sum(data_two_males_baffler$tactic == 'silent')

mate_call <- sum(data_single_male$mated == '1' & data_single_male$tactic == 'call')
total_call <- sum(data_single_male$tactic == 'call')

mate_call_with_sat <- sum(data_two_males_caller$mated == '1' & data_two_males_caller$tactic == 'call')
total_call_with_sat<- sum(data_two_males_caller$tactic == 'call')

mate_baff_with_sat <- sum(data_two_males_baffler$mated == '1' & data_two_males_baffler$tactic == 'baffling')
total_baff_with_sat <- sum(data_two_males_baffler$tactic == 'baffling')

# # Counting number of successful matings and total trials for treatments involving only naive males
# mate_silent_non_repeating_males <- sum(data_single_male_non_repeating_males$mated == '1' & data_single_male_non_repeating_males$tactic == 'silent')
# total_silent_non_repeating_males <- sum(data_single_male_non_repeating_males$tactic == 'silent')
# 
# mate_sat_with_cal_non_repeating_males <- sum(data_two_males_caller_non_repeating_males$mated == '1' & data_two_males_caller_non_repeating_males$tactic == 'silent')
# total_sat_with_cal_non_repeating_males <- sum(data_two_males_caller_non_repeating_males$tactic == 'silent')
# 
# mate_call_non_repeating_males <- sum(data_single_male_non_repeating_males$mated == '1' & data_single_male_non_repeating_males$tactic == 'call')
# total_call_non_repeating_males <- sum(data_single_male_non_repeating_males$tactic == 'call')
# 
# mate_call_with_sat_non_repeating_males <- sum(data_two_males_caller_non_repeating_males$mated == '1' & data_two_males_caller_non_repeating_males$tactic == 'call')
# total_call_with_sat_non_repeating_males <- sum(data_two_males_caller_non_repeating_males$tactic == 'call')


## Results: Bootstrapping 10000 times to obtain 95% confidence intervals (Within-Bush) ####

# Single male treatment

# Making dummy variables
dat1 <- data_single_male
g1 <- c() 
g2 <- c()

# Running a 'for loop' for 10000 iterations
for (i in 1:10000){
  
  a <- c() # storing the 10000 values for callers
  for (j in 1:length(dat1$treatment[dat1$tactic=='call'])){
    a <- c(a,sample(dat1$mated[dat1$tactic=='call'],1,replace=T))} # sampling with replacement
  g1 <- c(g1,mean(a))
  
  b <- c() # storing the 10000 values for silent males
  for (j in 1:length(dat1$treatment[dat1$tactic=='silent'])){
    b <- c(b,sample(dat1$mated[dat1$tactic=='silent'],1,replace=T))} # sampling with replacement
  g2 <- c(g2,mean(b))
  
}

mean_g1 <- mean(g1)
mean_g2 <- mean(g2)
# Calculating the 95% confidence intervals from the bootstrapped values
cis_g1<-quantile(g1,probs=c(0.025,0.975))
cis_g2<-quantile(g2,probs=c(0.025,0.975))


# Two male treatment (callers, satellites, or silent males) 

# Making dummy variables
dat2 <- data_two_males_caller
h1 <- c() 
h2 <- c()

# Running a 'for loop' for 10000 iterations
for (i in 1:10000){
  
  a <- c() # storing the 10000 values for callers
  for (j in 1:length(dat2$treatment[dat2$tactic=='call'])){
    a <- c(a,sample(dat2$mated[dat2$tactic=='call'],1,replace=T))} # sampling with replacement
  h1 <- c(h1,mean(a))
  
  b <- c() # storing the 10000 values for silent males
  for (j in 1:length(dat2$treatment[dat2$tactic=='silent'])){
    b <- c(b,sample(dat2$mated[dat2$tactic=='silent'],1,replace=T))} # sampling with replacement
  h2 <- c(h2,mean(b))
  
}

mean_h1 <- mean(h1)
mean_h2 <- mean(h2)
# Calculating the 95% confidence intervals from the bootstrapped values
cis_h1<-quantile(h1,probs=c(0.025,0.975))
cis_h2<-quantile(h2,probs=c(0.025,0.975))


# Two males (bafflers and satellites)

# Making dummy variables
dat3 <- data_two_males_baffler
i1 <- c() 
i2 <- c()

# Running a 'for loop' for 10000 iterations
for (i in 1:10000){
  
  a <- c() # storing the 10000 values for bafflers
  for (j in 1:length(dat3$tactic[dat3$tactic=='baffling'])){
    a <- c(a,sample(dat3$mated[dat3$tactic=='baffling'],1,replace=T))} # sampling with replacement
  i1 <- c(i1,mean(a))
  
  b <- c() # storing the 10000 values for silent males
  for (j in 1:length(dat3$tactic[dat3$tactic=='silent'])){
    b <- c(b,sample(dat3$mated[dat3$tactic=='silent'],1,replace=T))} # sampling with replacement
  i2 <- c(i2,mean(b))
  
}

mean_i1 <- mean(i1)
mean_i2 <- mean(i2)
# Calculating the 95% confidence intervals from the bootstrapped values
cis_i1<-quantile(i1,probs=c(0.025,0.975))
cis_i2<-quantile(i2,probs=c(0.025,0.975))

## Supplementary: Within-bush experiments with mated females ####

# Importing the data and subsetting 
data_mated_female_same_bush <- read.csv('data_mated_female_same_bush.csv')

data_single_male_M <- data_mated_female_same_bush %>% 
  filter(treatment == 'single_male')
data_two_males_M <- data_mated_female_same_bush %>% 
  filter(treatment == 'two_males')

# Calculating mating success
mate_silent_M <- sum(data_single_male_M$mated == '1' & data_single_male_M$tactic == 'silent')
total_silent_M <- sum(data_single_male_M$tactic == 'silent')

mate_sat_with_cal_M <- sum(data_two_males_M$mated == '1' & data_two_males_M$tactic == 'silent')
total_sat_with_cal_M <- sum(data_two_males_M$tactic == 'silent')

mate_call_M <- sum(data_single_male_M$mated == '1' & data_single_male_M$tactic == 'call')
total_call_M <- sum(data_single_male_M$tactic == 'call')

mate_call_with_sat_M <- sum(data_two_males_M$mated == '1' & data_two_males_M$tactic == 'call')
total_call_with_sat_M <- sum(data_two_males_M$tactic == 'call')

# Make a table with above data
results_table_M <- data.frame(
  Tactic = c(
    "Silent",
    "Satellite with Caller",
    "Caller",
    "Caller with Satellite"
  ),
  Mated = c(
    mate_silent_M,
    mate_sat_with_cal_M,
    mate_call_M,
    mate_call_with_sat_M
  ),
  Total = c(
    total_silent_M,
    total_sat_with_cal_M,
    total_call_M,
    total_call_with_sat_M
  )
)


# Bootstrapping to get confidence intervals

# single male

# Making dummy variables
dat1_M <- data_single_male_M
j1 <- c() 
j2 <- c()

# Running a 'for loop' for 10000 iterations
for (i in 1:10000){
  
  a <- c() # storing the 10000 values for callers
  for (j in 1:length(dat1_M$treatment[dat1_M$tactic=='call'])){
    a <- c(a,sample(dat1_M$mated[dat1_M$tactic=='call'],1,replace=T))} # sampling with replacement
  j1 <- c(j1,mean(a))
  
  b <- c() # storing the 10000 values for silent males
  for (j in 1:length(dat1_M$treatment[dat1_M$tactic=='silent'])){
    b <- c(b,sample(dat1_M$mated[dat1_M$tactic=='silent'],1,replace=T))} # sampling with replacement
  j2 <- c(j2,mean(b))
  
}

mean_j1 <- mean(j1)
mean_j2 <- mean(j2)
cis_j1<-quantile(j1,probs=c(0.025,0.975))
cis_j2<-quantile(j2,probs=c(0.025,0.975))



# two males (callers, satellites, and silent males) 

# Making dummy variables
dat2_M <- data_two_males_M
k1 <- c() 
k2 <- c()

# Running a 'for loop' for 10000 iterations
for (i in 1:10000){
  
  a <- c() # storing the 10000 values for callers
  for (j in 1:length(dat2_M$treatment[dat2_M$tactic=='call'])){
    a <- c(a,sample(dat2_M$mated[dat2_M$tactic=='call'],1,replace=T))} # sampling with replacement
  k1 <- c(k1,mean(a))
  
  b <- c() # storing the 10000 values for silent males
  for (j in 1:length(dat2_M$treatment[dat2_M$tactic=='silent'])){
    b <- c(b,sample(dat2_M$mated[dat2_M$tactic=='silent'],1,replace=T))} # sampling with replacement
  k2 <- c(k2,mean(b))
  
}

mean_k1 <- mean(k1)
mean_k2 <- mean(k2)
cis_k1<-quantile(k1,probs=c(0.025,0.975))
cis_k2<-quantile(k2,probs=c(0.025,0.975))


### Across-Bush Experiments ####

# Main data importing for across bush, virgin female experiments

data_single_male_across <- read.csv('data_across_bush_single_male.csv')
data_two_males_caller_across <- read.csv('data_across_bush_two_male.csv')
data_two_males_baffler_across <- read.csv('data_across_bush_baffler.csv')

## Calculating mating success (Across-Bush) ####

# Counting number of successful matings and total trials for the 6 treatments (Fig 4A, 4B, 5)
mate_silent_across <- sum(data_single_male_across$mated == '1' & data_single_male_across$tactic == 'silent')
total_silent_across <- sum(data_single_male_across$tactic == 'silent')

mate_sat_with_cal_across <- sum(data_two_males_caller_across$mated == '1' & data_two_males_caller_across$tactic== 'silent')
total_sat_with_cal_across <- sum(data_two_males_caller_across$tactic == 'silent')

mate_cal_with_baf_across <- sum(data_two_males_baffler_across$mated == '1' & data_two_males_baffler_across$tactic == 'silent')
total_cal_with_baf_across <- sum(data_two_males_baffler_across$tactic == 'silent')

mate_call_across <- sum(data_single_male_across$mated == '1' & data_single_male_across$tactic == 'call')
total_call_across <- sum(data_single_male_across$tactic == 'call')

mate_call_with_sat_across <- sum(data_two_males_caller_across$mated == '1' & data_two_males_caller_across$tactic == 'call')
total_call_with_sat_across <- sum(data_two_males_caller_across$tactic == 'call')

mate_baff_with_cal_across <- sum(data_two_males_baffler_across$mated == '1' & data_two_males_baffler_across$tactic == 'baffling')
total_baff_with_cal_across <- sum(data_two_males_baffler_across$tactic == 'baffling')


## Results: Bootstrapping 10000 times to obtain 95% confidence intervals (Across-Bush) ####

# Single male treatment

# Making dummy variables
dat4 <- data_single_male_across
u1 <- c() 
u2 <- c()

# Running a 'for loop' for 10000 iterations
for (i in 1:10000){
  
  a <- c() # storing the 10000 values for callers
  for (j in 1:length(dat4$treatment[dat4$tactic=='call'])){
    a <- c(a,sample(dat4$mated[dat4$tactic=='call'],1,replace=T))} # sampling with replacement
  u1 <- c(u1,mean(a))
  
  b <- c() # storing the 10000 values for silent males
  for (j in 1:length(dat4$treatment[dat4$tactic=='silent'])){
    b <- c(b,sample(dat4$mated[dat4$tactic=='silent'],1,replace=T))} # sampling with replacement
  u2 <- c(u2,mean(b))
  
}

mean_u1 <- mean(u1)
mean_u2 <- mean(u2)
# Calculating the 95% confidence intervals from the bootstrapped values
cis_u1<-quantile(u1,probs=c(0.025,0.975))
cis_u2<-quantile(u2,probs=c(0.025,0.975))


# Two male treatment (callers, satellites, or silent males) 

# Making dummy variables
dat5 <- data_two_males_caller_across
v1 <- c() 
v2 <- c()

# Running a 'for loop' for 10000 iterations
for (i in 1:10000){
  
  a <- c() # storing the 10000 values for callers
  for (j in 1:length(dat5$treatment[dat5$tactic=='call'])){
    a <- c(a,sample(dat5$mated[dat5$tactic=='call'],1,replace=T))} # sampling with replacement
  v1 <- c(v1,mean(a))
  
  b <- c() # storing the 10000 values for silent males
  for (j in 1:length(dat5$treatment[dat5$tactic=='silent'])){
    b <- c(b,sample(dat5$mated[dat5$tactic=='silent'],1,replace=T))} # sampling with replacement
  v2 <- c(v2,mean(b))
  
}

mean_v1 <- mean(v1)
mean_v2 <- mean(v2)
# Calculating the 95% confidence intervals from the bootstrapped values
cis_v1<-quantile(v1,probs=c(0.025,0.975))
cis_v2<-quantile(v2,probs=c(0.025,0.975))


# Two male treatment (baffler vs caller)

# Making dummy variables
dat6 <- data_two_males_baffler_across
w1 <- c() 
w2 <- c()

# Running a 'for loop' for 10000 iterations
for (i in 1:10000){
  
  a <- c() # storing the 10000 values for bafflers
  for (j in 1:length(dat6$tactic[dat6$tactic=='baffling'])){
    a <- c(a,sample(dat6$mated[dat6$tactic=='baffling'],1,replace=T))} # sampling with replacement
  w1 <- c(w1,mean(a))
  
  b <- c() # storing the 10000 values for silent males
  for (j in 1:length(dat6$tactic[dat6$tactic=='call'])){
    b <- c(b,sample(dat6$mated[dat6$tactic=='call'],1,replace=T))} # sampling with replacement
  w2 <- c(w2,mean(b))
  
}

mean_w1 <- mean(w1)
mean_w2 <- mean(w2)
# Calculating the 95% confidence intervals from the bootstrapped values
cis_w1<-quantile(w1,probs=c(0.025,0.975))
cis_w2<-quantile(w2,probs=c(0.025,0.975))



### Compiling Bootstrapping Results  ####

# Single male within-bush
summary_single_male <- data.frame( 
  Treatment = c("Within bush", "Within bush"),
  X.label = c("Caller", "Silent"),
  Female.mating.status = c("Virgin", "Virgin"),
  Strategy =c("Calling", "Silent"),
  Mean = c(mean_g1, mean_g2),
  X95._CI_min = c(cis_g1[1], cis_g2[1]),
  X95._CI_max = c(cis_g1[2], cis_g2[2]),
  stringsAsFactors = FALSE
)

# Two males within-bush with callers and satellites
summary_two_males_caller <- data.frame(
  Treatment = c("Within bush", "Within bush"),
  X.label = c("Caller with Satellite", "Satellite with Caller"),
  Female.mating.status = c("Virgin", "Virgin"),
  Strategy =c("Calling", "Silent"),
  Mean = c(mean_h1, mean_h2),
  X95._CI_min = c(cis_h1[1], cis_h2[1]),
  X95._CI_max = c(cis_h1[2], cis_h2[2]),
  stringsAsFactors = FALSE
)

# Two males within-bush with bafflers and satellites
summary_two_males_baffler <- data.frame(
  Treatment = c("Within bush", "Within bush"),
  X.label = c("Baffler with Satellite", "Satellite with Baffler"),
  Female.mating.status = c("Virgin", "Virgin"),
  Strategy =c("Calling", "Silent"),
  Mean = c(mean_i1, mean_i2),
  X95._CI_min = c(cis_i1[1], cis_i2[1]),
  X95._CI_max = c(cis_i1[2], cis_i2[2]),
  stringsAsFactors = FALSE
)

# Single male with mated female
summary_single_male_M <- data.frame(
  Treatment = c("Within bush", "Within bush"),
  X.label = c("Caller", "Silent"),
  Female.mating.status = c("Mated", "Mated"),
  Strategy =c("Calling", "Silent"),
  Mean = c(mean_j1, mean_j2),
  X95._CI_min = c(cis_j1[1], cis_j2[1]),
  X95._CI_max = c(cis_j1[2], cis_j2[2]),
  stringsAsFactors = FALSE
)

# Two males with mated female
summary_two_males_M <- data.frame(
  Treatment = c("Within bush", "Within bush"),
  X.label = c("Caller with Satellite", "Satellite with Caller"),
  Female.mating.status = c("Mated", "Mated"),
  Strategy =c("Calling", "Silent"),
  Mean = c(mean_k1, mean_k2),
  X95._CI_min = c(cis_k1[1], cis_k2[1]),
  X95._CI_max = c(cis_k1[2], cis_k2[2]),
  stringsAsFactors = FALSE
)

# Single male across-bush
summary_single_male_across <- data.frame(
  Treatment = c("Across bush", "Across bush"),
  X.label = c("Caller", "Silent"),
  Female.mating.status = c("Virgin", "Virgin"),
  Strategy =c("Calling", "Silent"),
  Mean = c(mean_u1, mean_u2),
  X95._CI_min = c(cis_u1[1], cis_u2[1]),
  X95._CI_max = c(cis_u1[2], cis_u2[2]),
  stringsAsFactors = FALSE
)

# Two males across-bush with callers and satellites
summary_two_males_caller_across <- data.frame(
  Treatment = c("Across bush", "Across bush"),
  X.label = c("Caller with Satellite", "Satellite with Caller"),
  Female.mating.status = c("Virgin", "Virgin"),
  Strategy =c("Calling", "Silent"),
  Mean = c(mean_v1, mean_v2),
  X95._CI_min = c(cis_v1[1], cis_v2[1]),
  X95._CI_max = c(cis_v1[2], cis_v2[2]),
  stringsAsFactors = FALSE
)

# Two males across-bush with bafflers and callers 
summary_two_males_baffler_across <- data.frame(
  Treatment = c("Across bush baffler", "Across bush baffler"),
  X.label = c("Baffler", "Caller"),
  Female.mating.status = c("Virgin", "Virgin"),
  Strategy =c("Calling", "Calling"),
  Mean = c(mean_w1, mean_w2),
  X95._CI_min = c(cis_w1[1], cis_w2[1]),
  X95._CI_max = c(cis_w1[2], cis_w2[2]),
  stringsAsFactors = FALSE
)


# Table 1: For Within-Bush experimental trials (Comparison between Silent vs Satellite) ####
dataset <- read.csv("data_mating_success_all_tactics_same_bush.csv")
dataset$tactic <- ifelse(dataset$tactic=='call','caller',dataset$tactic)
dataset <- dataset %>%
  mutate(tactic = case_when(
    treatment == 'two_males_baffler' & tactic == 'silent' ~ "satellite with baffler",
    treatment == 'two_males_baffler' & tactic == 'baffling' ~ "baffler with satellite",
    treatment == 'two_males_caller' & tactic == 'silent' ~ "satellite with caller",
    treatment == 'two_males_caller' & tactic == 'caller' ~ "caller with satellite",
    TRUE ~ tactic
  ))
dataset$tactic <- as.factor (dataset$tactic)

df_silent_vs_satellite <- filter(dataset,tactic =='silent' | tactic=='satellite with caller' | tactic=='satellite with baffler') 
df_signaller_vs_signaller_w_satellite <- filter(dataset,tactic =='caller' | tactic=='caller with satellite'  | tactic=='baffler with satellite' ) 

df_silent_vs_satellite <- df_silent_vs_satellite %>%
  left_join(
    df_signaller_vs_signaller_w_satellite %>%
      dplyr::select(exp_ids, partner_maleid = maleid),
    by = "exp_ids"
  )
df_signaller_vs_signaller_w_satellite <- df_signaller_vs_signaller_w_satellite %>%
  left_join(
    df_silent_vs_satellite %>%
      dplyr::select(exp_ids, partner_maleid = maleid),
    by = "exp_ids"
  )

df_silent_vs_satellite$tactic <- factor(df_silent_vs_satellite$tactic,levels = c("silent", "satellite with caller", "satellite with baffler"))
df_silent_vs_satellite$partner_maleid <- ifelse(df_silent_vs_satellite$tactic=='silent','no_partner',df_silent_vs_satellite$partner_maleid)
mod <- glmer (mated~tactic + (1|maleid) + (1|partner_maleid)  ,family = binomial, data=df_silent_vs_satellite)
summary(mod)

mod <- glmer (mated~tactic + (1|maleid) ,family = binomial, data=df_silent_vs_satellite)
summary(mod)
s <- summary(mod)
coef_table <- as.data.frame(s$coefficients)
write.csv(coef_table, "./output/table1.csv", row.names = TRUE)

# Table 2: For Within-Bush experimental trials (Comparison between callers; callers and bafflers in presence of satellite.) ####

df_signaller_vs_signaller_w_satellite$tactic <- factor(df_signaller_vs_signaller_w_satellite$tactic,levels = c("caller", "caller with satellite", "baffler with satellite"))
df_signaller_vs_signaller_w_satellite$partner_maleid <- ifelse(df_signaller_vs_signaller_w_satellite$tactic=='caller','no_partner',df_silent_vs_satellite$partner_maleid)
mod2 <- glmer (mated~tactic+ (1|maleid) + (1|partner_maleid),family = binomial, data=df_signaller_vs_signaller_w_satellite)
summary(mod2)
mod2 <- glmer (mated~tactic+ (1|maleid),family = binomial, data=df_signaller_vs_signaller_w_satellite)
s <- summary(mod2)
coef_table <- as.data.frame(s$coefficients)
write.csv(coef_table, "./output/table2.csv", row.names = TRUE)

# Table 3: For Across-Bush experimental trials (Comparison of silent vs satellite) ####
d1<- read.csv('data_across_bush_single_male.csv')
d2 <- read.csv('data_across_bush_two_male.csv')
dataset<- rbind(d1,d2)
dataset$tactic <- ifelse(dataset$tactic=='call','caller',dataset$tactic)
dataset <- dataset %>%
  mutate(tactic = case_when(
    treatment == 'two_males' & tactic == 'silent' ~ "satellite with caller",
    treatment == 'two_males' & tactic == 'caller' ~ "caller with satellite",
    TRUE ~ tactic
  ))

dataset$tactic <- factor(dataset$tactic,
                         levels = c("silent",
                                    "satellite with caller",
                                    "caller",
                                    "caller with satellite"))
dataset$tactic <- as.factor (dataset$tactic)


df_silent_vs_satellite <- filter(dataset,tactic =='silent' | tactic=='satellite with caller') 
df_caller_vs_caller_w_satellite <- filter(dataset,tactic =='caller' | tactic=='caller with satellite')

df_silent_vs_satellite <- df_silent_vs_satellite %>%
  left_join(
    df_caller_vs_caller_w_satellite %>%
      dplyr::select(exp_ids, partner_maleid = maleid),
    by = "exp_ids"
  )
df_caller_vs_caller_w_satellite <- df_caller_vs_caller_w_satellite %>%
  left_join(
    df_silent_vs_satellite %>%
      dplyr::select(exp_ids, partner_maleid = maleid),
    by = "exp_ids"
  )

df_silent_vs_satellite$partner_maleid <- ifelse(df_silent_vs_satellite$tactic=='silent','no_partner',df_silent_vs_satellite$partner_maleid )
mod <- glmer (mated~ tactic + (1|maleid) + (1|partner_maleid),family = binomial, data=df_silent_vs_satellite) 
summary (mod)
mod <- glmer (mated~ tactic + (1|maleid),family = binomial, data=df_silent_vs_satellite)
s <- summary(mod)
coef_table <- as.data.frame(s$coefficients)
write.csv(coef_table, "./output/table3.csv", row.names = TRUE)


# Table 4: For Across-Bush experimental trials (Comparison of caller vs caller with satellite) ####
df_caller_vs_caller_w_satellite$partner_maleid <- ifelse(df_caller_vs_caller_w_satellite$tactic=='caller','no_partner',df_caller_vs_caller_w_satellite$partner_maleid) 
mod <- glmer (mated~tactic + (1|maleid) + (1|partner_maleid),family = binomial, data=df_caller_vs_caller_w_satellite)
summary (mod)
mod <- glmer (mated~tactic + (1|maleid),family = binomial, data=df_caller_vs_caller_w_satellite)

s <- summary(mod)
coef_table <- as.data.frame(s$coefficients)
write.csv(coef_table, "./output/table4.csv", row.names = TRUE)

### Importing data for plots ####
# Creating a master dataframe with all the 95% confidence interval data
within_bush_bootstrap_CI <- rbind(summary_single_male, summary_two_males_caller,
                                  summary_two_males_baffler, summary_single_male_M, 
                                  summary_two_males_M, summary_single_male_across,
                                  summary_two_males_caller_across, summary_two_males_baffler_across)

dataset <- within_bush_bootstrap_CI # storing plotting data in a dataframe 


## Fig. 3a: Plots for Within-Bush Silent Strategies ####

dat_silent <- filter(dataset,dataset$Treatment == 'Within bush', dataset$Female.mating.status == 'Virgin',
                     dataset$Strategy == "Silent")

dat_silent <- dat_silent %>% # making appropriate labels for the plot
  mutate(X.label = case_when(
    X.label == "Silent" ~ "Silent",
    X.label == "Satellite with Caller" ~ "Satellite with\nCaller",
    X.label == "Satellite with Baffler" ~ "Satellite with\nBaffler",
  ))


plot_silent <- dat_silent %>%
  ggplot(aes(x = factor(X.label, levels = c('Silent', 'Satellite with\nCaller', 'Satellite with\nBaffler')), 
             y = Mean)) +
  ylab('Mating success') +
  xlab('Tactic') +
  geom_point(aes(y = Mean), color = "black", size = 5) +
  geom_errorbar(aes(ymin = X95._CI_min, ymax = X95._CI_max), width = 0.3, size = 2, col = 'black') +
  theme_classic(base_size = 25) +
  theme(axis.text = element_text(face = 'bold'),
        axis.title = element_text(face = 'bold'), 
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28), 
        axis.text.x = element_text(size = 25)) +
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0.00, 0.25, 0.50, 0.75, 1.00))

ggsave("./plots/fig3a.png", plot = plot_silent , width = 10, height = 7)

## Fig. 3b: Plots for Within-Bush Calling Strategies ####

dat_call <- filter(dataset,dataset$Treatment == 'Within bush', dataset$Female.mating.status == 'Virgin',
                   dataset$Strategy == "Calling")

dat_call <- dat_call %>% # making appropriate labels for the plot
  mutate(X.label = case_when(
    X.label == "Caller" ~ "Caller",
    X.label == "Caller with Satellite" ~ "Caller with\nSatellite",
    X.label == "Baffler with Satellite" ~ "Baffler with\nSatellite",
  ))

plot_call <- dat_call %>%
  ggplot(aes(x = factor(X.label, levels = c('Caller', "Caller with\nSatellite", "Baffler with\nSatellite")), 
             y = Mean)) +
  ylab('Mating success') +
  xlab('Tactic') +
  geom_point(aes(y = Mean), color = "black", size = 5) +
  geom_errorbar(aes(ymin = X95._CI_min, ymax = X95._CI_max), width = 0.3, size = 2, col = 'black') +
  theme_classic(base_size = 25) +
  theme(axis.text = element_text(face = 'bold'),
        axis.title = element_text(face = 'bold'), 
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28), 
        axis.text.x = element_text(size = 25)) +
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0.00, 0.25, 0.50, 0.75, 1.00))

ggsave("./plots/fig3b.png", plot = plot_call , width = 10, height = 7)

## Fig. 4a: Plots for Across-Bush Silent Strategies ####

dat_across_sil <- filter(dataset,dataset$Treatment == 'Across bush', dataset$Female.mating.status == 'Virgin',
                         dataset$Strategy == "Silent")

dat_across_sil <- dat_across_sil %>% # making appropriate labels for the plot
  mutate(X.label = case_when(
    X.label == "Silent" ~ "Silent",
    X.label == "Satellite with Caller" ~ "Satellite with\nCaller",
  ))


plot_across_sil <- dat_across_sil  %>%
  ggplot(aes(x = factor(X.label, levels = c('Silent', 'Satellite with\nCaller')), 
             y = Mean)) +
  ylab('Mating success') +
  xlab('Tactic') +
  geom_point(aes(y = Mean), color = "black", size = 5) +
  geom_errorbar(aes(ymin = X95._CI_min, ymax = X95._CI_max), width = 0.3, size = 2, col = 'black') +
  theme_classic(base_size = 25) +
  theme(axis.text = element_text(face = 'bold'),
        axis.title = element_text(face = 'bold'), 
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28), 
        axis.text.x = element_text(size = 25)) +
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0.00, 0.25, 0.50, 0.75, 1.00))

ggsave("./plots/fig4a.png", plot = plot_across_sil , width = 10, height = 7)


## Fig. 4b: Plots for Across-Bush Calling Strategies ####

dat_across_cal <- filter(dataset,dataset$Treatment == 'Across bush', dataset$Female.mating.status == 'Virgin',
                         dataset$Strategy == "Calling")

dat_across_cal <- dat_across_cal %>% # making appropriate labels for the plot
  mutate(X.label = case_when(
    X.label == "Caller" ~ "Caller",
    X.label == "Caller with Satellite" ~ "Caller with\nSatellite",
  ))


plot_across_cal <- dat_across_cal  %>%
  ggplot(aes(x = factor(X.label, levels = c('Caller', "Caller with\nSatellite")), 
             y = Mean)) +
  ylab('Mating success') +
  xlab('Tactic') +
  geom_point(aes(y = Mean), color = "black", size = 5) +
  geom_errorbar(aes(ymin = X95._CI_min, ymax = X95._CI_max), width = 0.3, size = 2, col = 'black') +
  theme_classic(base_size = 25) +
  theme(axis.text = element_text(face = 'bold'),
        axis.title = element_text(face = 'bold'), 
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28), 
        axis.text.x = element_text(size = 25)) +
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0.00, 0.25, 0.50, 0.75, 1.00))

ggsave("./plots/fig4b.png", plot = plot_across_cal , width = 10, height = 7)


## Fig. 5a: Plots for Across-Bush Experiments with Bafflers ####

dat_baff_across <- filter(dataset,dataset$Treatment == 'Across bush baffler', dataset$Female.mating.status == 'Virgin',
                          dataset$Strategy == "Calling")


plot_baff_across <- dat_baff_across %>% 
  ggplot(aes(x = factor(X.label, levels = c('Caller', 'Baffler')), 
             y = Mean)) +
  ylab('Mating success') +
  xlab('Tactic') +
  geom_point(aes(y = Mean), color = "black", size = 5) +
  geom_errorbar(aes(ymin = X95._CI_min, ymax = X95._CI_max), width = 0.3, size = 2, col = 'black') +
  theme_classic(base_size = 25) +
  theme(axis.text = element_text(face = 'bold'),
        axis.title = element_text(face = 'bold'), 
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28), 
        axis.text.x = element_text(size = 25)) +
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0.00, 0.25, 0.50, 0.75, 1.00))

ggsave("./plots/fig5a.png", plot = plot_baff_across , width = 10, height = 7)

# GLMM to compare the mating success of bafflers and callers in across-bush experiments
mod <- glmer (mated~tactic + (1|maleid), family = binomial, data=data_two_males_baffler_across)
# summary(mod)
s <- summary(mod)
coef_table <- as.data.frame(s$coefficients)
write.csv(coef_table, "./output/table5.csv", row.names = TRUE)

# GLM to compare the mating success of bafflers and callers in across-bush experiments
# mod <- glm(mated ~ tactic, 
#            family = binomial, 
#            data = data_two_males_baffler_across)
# 
# s <- summary(mod)
# coef_table <- as.data.frame(s$coefficients)


## Fig. 5b: Plots for Across-Bush Perceived SPL ####
# import data
suppl_ab <- read.csv("./suppl_across_bush_baffler.csv", stringsAsFactors = T)

perc_SPL <- suppl_ab[,c(1,4,8)]
colnames(perc_SPL) <- c("Trial_ID", "Baffler", "Caller")

perc_long <- perc_SPL %>%
  pivot_longer(cols = -Trial_ID, names_to = "ART", values_to = "SPL")

plot_percSPL <- ggplot(perc_long, aes(x = ART, y = SPL, fill =ART)) +
  geom_boxplot(alpha = 0.85) +
  scale_fill_manual(values = c("#E69F00","#56B4E9")) +
  labs(fill = "",
       x = "Tactic",
       y = "Perceived SPL (dB)"
  ) +
  geom_line(aes(group = Trial_ID), color = "black", alpha = 0.7) + 
  geom_point(color = "black", size = 1.5, alpha = 0.5)+
  theme_minimal()+
  theme(legend.title = element_text(size = 14),  
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

ggsave("./plots/fig5b.png", plot = plot_percSPL , width = 10, height = 7)


## Table 6, 7 & Fig. 6: Effect of Initial Distance and Tactic on Male Mating Probability ####

## Proportion tests for estimating spatial effects on responses
## Comparing mating success between within-bush and across-bush experiments

dataset <- read.csv("data_mating_success_all_tactics.csv")

df_lone_silent <- dataset %>%
  filter(tactic == "silent",
         treatment == "single_male")

df_silent_satellite <- dataset %>%
  filter(tactic == "silent",
         treatment == "two_males_caller")

df_lone_caller <- dataset %>%
  filter(tactic == "call",
         treatment == "single_male")

df_caller_w_satellite <- dataset %>%
  filter(tactic == "call",
         treatment == "two_males_caller")

run_prop_test <- function(df, expt) {
  
  estimator <- df %>%
    group_by(experiment) %>%
    summarise(
      success = sum(mated == 1),
      total = n(),
      .groups = "drop"
    )
  
  test <- prop.test(estimator$success, estimator$total)
  
  data.frame(
    comparison = expt,
    mated_across = estimator$success[1],
    total_across = estimator$total[1],
    mated_within = estimator$success[2],
    total_within = estimator$total[2],
    prop1 = estimator$success[1] / estimator$total[1],
    prop2 = estimator$success[2] / estimator$total[2],
    chi_sq_statistic = test$statistic,
    p_value = test$p.value
  )
}
test1 <- run_prop_test(df_lone_silent, "lone_silent")
test2 <- run_prop_test(df_silent_satellite, "silent_satellite")
test3 <- run_prop_test(df_lone_caller, "lone_caller")
test4 <- run_prop_test(df_caller_w_satellite, "caller_with_satellite")

final_results <- bind_rows(test1, test2, test3, test4)

write.csv(final_results, "./output/spatial_responses_prop_tests.csv", row.names = FALSE)

# Table 6: Effect of initial distance to female on mating success of silent and satellite males ####
dataset <- read.csv('data_summary_virgin_female_same_bush.csv')
dataset<- filter(dataset,plant_id!='Plant_03') 
dataset$tactic <-ifelse(dataset$tactic=='call','caller',dataset$tactic)
dataset <- dataset %>%
  mutate(tactic = case_when(
    treatment == 'two_males' & tactic == 'silent' ~ "satellite",
    treatment == 'two_males' & tactic == 'caller' ~ "caller with satellite",
    TRUE ~ tactic
  ))
dataset$tactic <- as.factor(dataset$tactic)

data_silent_satellite <- filter(dataset,tactic =='silent' | tactic=='satellite' ) 
data_caller_w_satellite <- filter (dataset,tactic== 'caller' | tactic == 'caller with satellite')

data_silent_satellite <- data_silent_satellite %>%
  left_join(
    data_caller_w_satellite %>%
      dplyr::select(exp_ids, partner_maleid = maleid),
    by = "exp_ids"
  )
data_caller_w_satellite <- data_caller_w_satellite %>%
  left_join(
    data_silent_satellite %>%
      dplyr::select(exp_ids, partner_maleid = maleid),
    by = "exp_ids"
  )
data_silent_satellite$tactic <- relevel(data_silent_satellite$tactic, ref = "silent")
data_silent_satellite$partner_maleid <- ifelse(data_silent_satellite$tactic=='silent','no_partner',data_silent_satellite$partner_maleid)
mod1 <- glmer(mated ~ tactic * initial_dist_to_female + (1|maleid) + (1|partner_maleid),
              family = binomial(link = "logit"),
              data = data_silent_satellite )
summary(mod1)
mod1 <- glmer(mated ~ tactic * initial_dist_to_female + (1|maleid),
              family = binomial(link = "logit"),
              data = data_silent_satellite )
s <- summary(mod1)
coef_table <- as.data.frame(s$coefficients)
write.csv(coef_table, "./output/table6.csv", row.names = TRUE)


pred_link <- stats::predict(mod1, type = "link", se.fit = TRUE)
fit <- pred_link$fit
se  <- pred_link$se.fit

data_silent_satellite$predicted <- plogis(fit)
data_silent_satellite$ci_lower  <- plogis(fit - 1.96 * se)
data_silent_satellite$ci_upper  <- plogis(fit + 1.96 * se)

dat_bubble <- data_silent_satellite %>%
  group_by(tactic, initial_dist_to_female, mated) %>%
  summarise(count = n(), .groups = 'drop')

# Fig 6a. Effect of distance to female and tactic on mating success of silent and satellite males ####
p1 <- ggplot() +
  # Raw observed data as colored, semi-transparent bubbles
  geom_point(data = dat_bubble,
             aes(x = initial_dist_to_female,
                 y = mated,
                 size = count,
                 color = tactic),
             alpha = 0.6) +  # transparency for overlap visibility
  
  # Confidence intervals around predicted probabilities
  geom_errorbar(data = data_silent_satellite,
                aes(x = initial_dist_to_female,
                    ymin = ci_lower,
                    ymax = ci_upper,
                    color = tactic),
                width = 0.15, size = 1.2, alpha = 0.7) +
  
  # Predicted points joined by lines
  geom_line(data = data_silent_satellite,
            aes(x = initial_dist_to_female,
                y = predicted,
                color = tactic,
                group = tactic),
            size = 2) +
  
  # Predicted points themselves
  geom_point(data = data_silent_satellite,
             aes(x = initial_dist_to_female,
                 y = predicted,
                 color = tactic),
             size = 3) +
  
  # Bubble size legend
  scale_size_continuous(
    name = "Number of \nobservations",
    breaks = c(2, 4, 6, 8),
    range = c(3, 12),
    limits = c(1, max(dat_bubble$count, na.rm = TRUE))
  ) +
  
  labs(x = 'Distance to female (au)',
       y = 'Mating success',
       color = 'Tactic') +
  theme_classic(base_size = 25) +
  guides(
    color = guide_legend(order = 1),   # Tactic (color) on top
    size  = guide_legend(order = 2)    # Number of observations below
  )



# Table 7: Effect of initial distance to female on mating success of callers and callers with satellites. ####

data_caller_w_satellite$tactic <- factor(
  data_caller_w_satellite$tactic,
  levels = c("caller", "caller with satellite"),
  labels = c("caller", "caller with\nsatellite")
)

data_caller_w_satellite$partner_maleid <- ifelse(data_caller_w_satellite$tactic=='caller','no_partner',data_caller_w_satellite$partner_maleid)
mod1 <- glmer(mated ~ tactic * initial_dist_to_female + (1|maleid) + (1|partner_maleid) ,
              family = binomial(link = "logit"),
              data = data_caller_w_satellite)
summary(mod1)
mod1 <- glmer(mated ~ tactic * initial_dist_to_female + (1|maleid)  ,
              family = binomial(link = "logit"),
              data = data_caller_w_satellite)
# Model summary
summary(mod1)
s <- summary(mod1)
coef_table <- as.data.frame(s$coefficients)
write.csv(coef_table, "./output/table7.csv", row.names = TRUE)

pred_link <- stats::predict(mod1, type = "link", se.fit = TRUE)
fit <- pred_link$fit
se  <- pred_link$se.fit

data_caller_w_satellite$predicted <- plogis(fit)
data_caller_w_satellite$ci_lower  <- plogis(fit - 1.96 * se)
data_caller_w_satellite$ci_upper  <- plogis(fit + 1.96 * se)

dat_bubble <- data_caller_w_satellite %>%
  group_by(tactic, initial_dist_to_female, mated) %>%
  summarise(count = n(), .groups = 'drop')

# Fig 6b. Effect of distance to female and tactic on mating success  of callers and callers with satellite. ####
p2 <- ggplot() +
  # Raw observed data as colored, semi-transparent bubbles
  geom_point(data = dat_bubble,
             aes(x = initial_dist_to_female,
                 y = mated,
                 size = count,
                 color = tactic),
             alpha = 0.6) +  
  
  # Confidence intervals around predicted probabilities
  geom_errorbar(data = data_caller_w_satellite,
                aes(x = initial_dist_to_female,
                    ymin = ci_lower,
                    ymax = ci_upper,
                    color = tactic),
                width = 0.15, size = 1.2, alpha = 0.7) +
  
  # Predicted points joined by lines
  geom_line(data = data_caller_w_satellite,
            aes(x = initial_dist_to_female,
                y = predicted,
                color = tactic,
                group = tactic),
            size = 2) +
  
  # Predicted points themselves
  geom_point(data = data_caller_w_satellite,
             aes(x = initial_dist_to_female,
                 y = predicted,
                 color = tactic),
             size = 3) +
  
  # Bubble size legend
  scale_size_continuous(
    name = "Number of \nobservations",
    breaks = c(2, 4, 6, 8),
    range = c(3, 12),
    limits = c(1, max(dat_bubble$count, na.rm = TRUE))
  ) +
  
  labs(x = 'Distance to female (au)',
       y = 'Mating success',
       color = 'Tactic') +
  theme_classic(base_size = 25) +
  guides(
    color = guide_legend(order = 1),   
    size  = guide_legend(order = 2)    
  )

# Fig. 6: Combining plots 6a and 6b.
final_plot <- p1 + p2 +
  plot_annotation(tag_levels = 'a') &
  theme(
    plot.tag = element_text(face = "bold", size = 32),
    plot.tag.position = c(0.02, 0.98)  
  )

final_plot

ggsave(
  filename = "./plots/fig6.png",
  plot = final_plot,
  device = "tiff",
  width = 18,
  height = 8,
  units = "in",
  dpi = 300,
  compression = "lzw"   # recommended for journals
)


# Fig. 7a: Female Acoustic Choice as a function of relative distance of male from female ####
acoustic_choice <- suppl_ab %>% 
  subset(Female_AcousticChoice != "NONE" & !is.na(DistanceFromFemale_Male2_cm)) %>% # removing trial which doesn't have distance data
  mutate(Decision = ifelse(Female_AcousticChoice == "Male1", 1,0)) 
plot_acoustic_choice <- ggplot(acoustic_choice,
                               aes(x = Difference_Distance_cm, y = Decision)) +
  geom_jitter(height = 0.05, width = 0.05,
              alpha = 0.6, size = 2, color = "black") +
  
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = TRUE,
              color = "black",
              size = 2,
              fill = "grey70",
              alpha = 0.4) +
  
  scale_y_continuous(limits = c(-0.2, 1.2),
                     breaks = seq(0, 1, 0.2)) +
  
  labs(x = "Relative distance to female\n(Baffler − Caller, cm)",
       y = "Female acoustic choice\n(Baffler = 1, Caller = 0)") +
  
  theme_classic(base_size = 25) +
  theme(
    axis.title = element_text(size = 25),
    axis.text  = element_text(size = 22)
  )

# Fit the binomial logistic regression model
binomial_model <- glm(Decision ~ Difference_Distance_cm, data = acoustic_choice, family = binomial)

# Fig. 7b: Comparing distance from female of bafflers and callers ####
distance <- suppl_ab %>% 
  subset(!is.na(DistanceFromFemale_Male2_cm))  # removing trial which doesn't have distance data

distance <- distance[,c(1,5,9)] # selecting requisite columns
colnames(distance) <- c("Trial_ID", "Baffler", "Caller")

dist_long <- distance %>%
  pivot_longer(cols = -Trial_ID, names_to = "ART", values_to = "Dist")

plot_dist <- ggplot(dist_long,
                    aes(x = ART, y = Dist, color = ART)) +
  
  geom_boxplot(alpha = 0.6,
               width = 0.5,
               outlier.shape = NA,
               size = 1.5) +
  
  geom_line(aes(group = Trial_ID),
            color = "black",
            alpha = 0.5,
            size = 1) +
  
  geom_point(size = 2.5,
             alpha = 0.7,
             color = "black") +
  
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  
  labs(x = "Tactic",
       y = "Distance from female (cm)",
       color = "Tactic") +
  
  theme_classic(base_size = 25) +
  theme(
    axis.title = element_text(size = 25),
    axis.text  = element_text(size = 22),
    legend.position = "none"  
  )


final_plot <- plot_acoustic_choice + plot_dist +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a') &
  theme(
    plot.tag = element_text(face = "bold", size = 32),
    plot.tag.position = c(0.02, 0.98)
  )

final_plot
ggsave("./plots/fig7.png", plot = final_plot,
       width = 12, height = 8, dpi = 300)

# Paired t-test for comparing the difference in perceived SPL of callers and bafflers for female
test1 <- t.test(suppl_ab$Perceived_SPL_Male1_dB, suppl_ab$Perceived_SPL_Male2_dB, paired = TRUE)

# Paired t-test to compare distance from female of callers and bafflers in across-bush experiments
test2 <- t.test(suppl_ab$DistanceFromFemale_Male1_cm, suppl_ab$DistanceFromFemale_Male2_cm, paired = TRUE)


## Output the paired t-test results as a .csv file
t.test.results <- data.frame(
  test = c("Test 1: Perceived SPL (dB)", "Test 2: Distance from female (cm)"),
  t_statistic = c(test1$statistic, test2$statistic),
  df = c(test1$parameter, test2$parameter),
  p_value = c(test1$p.value, test2$p.value),
  difference = c(test1$estimate, test2$estimate),
  CI_low = c(test1$conf.int[1], test2$conf.int[1]),
  CI_high = c(test1$conf.int[2], test2$conf.int[2])
)

write.csv(t.test.results, "./output/paired_t_test_results.csv", row.names = FALSE)



###############################################################################################
#                                Supplementary plots                                          #
###############################################################################################


## Fig. S2: Plots for Within-bush calling effort ####
dataset1 <- read.csv('data_summary_virgin_female_same_bush.csv')
dat <- filter(dataset1,tactic=='call')

png("./plots/figS2.png", width = 1000, height = 700)  # Open PNG device
hist(dat$call_effort,
     main = "",
     xlab = "Call Effort",
     ylab = "Number of males",
     border = "black")
dev.off()
median(dat$call_effort)
mean(dat$call_effort)
sd(dat$call_effort)
table(dat$call_number==1) #sample size


## Fig. S3: Plots for Across-bush calling effort ####
d1<- read.csv('data_across_bush_single_male.csv')
d2 <- read.csv('data_across_bush_two_male.csv')
dataset2<- rbind(d1,d2)
dat <- filter(dataset2,tactic=='call')
dat$call_effort <-dat$number_of_scans_called/dat$Duration_of._trial_.min.
png("./plots/figS3.png", width = 1000, height = 700)  # Open PNG device
hist(dat$call_effort,
     main = "",
     xlab = "Call Effort",
     ylab = "Number of males",
     border = "black")
dev.off()
median (dat$call_effort)
mean(dat$call_effort)
sd(dat$call_effort)
table(dat$number_of_scans_called==1) #sample size




## Fig. S4a: Plots for Within-bush Silent Strategies (Mated) ####
dataset <- within_bush_bootstrap_CI # storing plotting data in a dataframe
dat_silentM <- filter(dataset,dataset$Treatment == 'Within bush', dataset$Female.mating.status == 'Mated',
                      dataset$Strategy == "Silent")

dat_silentM <- dat_silentM %>% # making appropriate labels for the plot
  mutate(X.label = case_when(
    X.label == "Silent" ~ "Silent",
    X.label == "Satellite with Caller" ~ "Satellite with\nCaller"
  ))


plot_silentM <- dat_silentM %>%
  ggplot(aes(x = factor(X.label, levels = c('Silent', 'Satellite with\nCaller')), 
             y = Mean)) +
  ylab('Mating success') +
  xlab('Tactic') +
  geom_point(aes(y = Mean), color = "black", size = 5) +
  geom_errorbar(aes(ymin = X95._CI_min, ymax = X95._CI_max), width = 0.3, size = 2, col = 'black') +
  theme_classic(base_size = 25) +
  theme(axis.text = element_text(face = 'bold'),
        axis.title = element_text(face = 'bold'), 
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28), 
        axis.text.x = element_text(size = 25)) +
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0.00, 0.25, 0.50, 0.75, 1.00))

ggsave("./plots/figS4A.png", plot = plot_silentM , width = 10, height = 7)

#Proportion test for difference between silent and satellites
# prop.test(c(0,2), c(9,14), p = NULL, alternative = "two.sided",
#           correct = TRUE)

## Fig. S4b: Plots for Within-bush Calling Strategies (Mated)####
dataset <- within_bush_bootstrap_CI
dat_callM <- filter(dataset,dataset$Treatment == 'Within bush', dataset$Female.mating.status == 'Mated',
                    dataset$Strategy == "Calling")

dat_callM <- dat_callM %>% # making appropriate labels for the plot
  mutate(X.label = case_when(
    X.label == "Caller" ~ "Caller",
    X.label == "Caller with Satellite" ~ "Caller with\nSatellite"
  ))

plot_callM <- dat_callM %>%
  ggplot(aes(x = factor(X.label, levels = c('Caller', "Caller with\nSatellite")), 
             y = Mean)) +
  ylab('Mating success') +
  xlab('Tactic') +
  geom_point(aes(y = Mean), color = "black", size = 5) +
  geom_errorbar(aes(ymin = X95._CI_min, ymax = X95._CI_max), width = 0.3, size = 2, col = 'black') +
  theme_classic(base_size = 25) +
  theme(axis.text = element_text(face = 'bold'),
        axis.title = element_text(face = 'bold'), 
        axis.title.x = element_text(size =  28),
        axis.title.y = element_text(size = 28), 
        axis.text.x = element_text(size = 25)) +
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0.00, 0.25, 0.50, 0.75, 1.00))

ggsave("./plots/figS4B.png", plot = plot_callM , width = 10, height = 7)

#Proportion test for difference between callers and callers with satellites
# prop.test(c(1,5), c(14,21), p = NULL, alternative = "two.sided",
#           correct = TRUE)





## Fig. S5, S6: Plots for Pilot Experiments ####
# import data
pilot <- read.csv("./suppl_pilot.csv", stringsAsFactors = T)

pilot_1 <- pilot 

# Histogram for Perceived SPL of Male 1 for Male 2
plot_pilot_1 <- ggplot(pilot_1, aes(x = Perceived_SPL_Male12_dB)) +
  geom_histogram(bins = 50, fill = "#56B4E9", color = "black") +
  labs(title = "Histogram of perceived SPL for other male", x = "Perceived intensity (dB SPL)", 
       y = "Frequency of simultaneous callers") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  ) +
  scale_x_continuous(breaks = seq(min(pilot$Perceived_SPL_Male12_dB), max(pilot$Perceived_SPL_Male12_dB), by = 5)) +
  scale_y_continuous(breaks = seq(0, max(table(cut(pilot$Perceived_SPL_Male12_dB, breaks = 50))), by = 10))

ggsave("./plots/figS5.png", plot = plot_pilot_1 , width = 10, height = 7)


pilot_2 <- pilot %>% 
  subset(Perceived_SPL_Male12_dB > 53 & Perceived_SPL_Male12_dB < 57) # subset data

# Histogram for Distance between two plants (when perceived SPL = 55 dB)
plot_pilot_2 <- ggplot(pilot_2, aes(x = Distance_bw_plants12_cm)) +
  geom_histogram(bins = 50, fill = "#56B4E9", color = "black") +
  labs(title = "Distance between plants when perceived SPL = 55 dB", x = "Distance (cm)", 
       y = "Frequency of simultaneous callers") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  ) 

ggsave("./plots/figS6.png", plot = plot_pilot_2 , width = 10, height = 7)


## Fig. S7: Supplementary Plots for Across-Bush Experiments ####

# Fig. S7: Female Decision Latency as a function of relative perceived loudness of males 
latency <- suppl_ab %>% 
  subset(Female_AcousticChoice != "NONE") 

plot_latency <- ggplot(latency, aes(x = Difference_PerceivedSPL_dB, y = Decision_Latency_min)) + 
  geom_point() +
  labs(
    title = "",
    x = "Difference in Perceived SPL (dB)",
    y = "Latency (min)"
  ) +
  geom_smooth(method = "lm", color = "blue", size = 1) +  
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14)) +
  theme_minimal()

lm.latency <- lm(Decision_Latency_min ~ Difference_PerceivedSPL_dB, data = latency)

ggsave("./plots/figS7.png", plot = plot_latency , width = 10, height = 7)




## Fig. S8, S9: Plots for Calling Propensity as a function of body size ####
# Fig. S10: Body size vs calling probability (single and two males) 
d1<- read.csv('data_across_bush_single_male.csv')
d2 <- read.csv('data_across_bush_two_male.csv')
dat<- rbind(d1,d2)
dat$tactic_binary <- ifelse(dat$tactic=='call',1,0)
dat <- dat[!is.na(dat$Mean_body_length_male.mm.), ]

mod <- lm(tactic_binary~Mean_body_length_male.mm., data = dat)
summary(mod)


dat$predicted <- predict(mod, type = "response")
my_plot <- dat%>%
  ggplot(aes(x=Mean_body_length_male.mm.,y=tactic_binary)) +
  xlab('Body size (mm)') +
  ylab('Calling probability') +
  geom_jitter(size=5,width=0.2,height=0.01,alpha=0.8) +
  # geom_smooth(method = "lm", color = "blue", size = 1, se = FALSE)+
  geom_line(aes(y=predicted),size=2) +
  #geom_errorbar(aes(ymin=ci_min, ymax=ci_max), width=0.3,size=1,col='blue') +
  theme_classic(base_size = 25)
ggsave("./plots/figS8.png", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

# Fig. S9: Body size vs calling probability for across-bush trials (single male)
d1<- read.csv('data_across_bush_single_male.csv')
dat<- d1
dat$tactic_binary <- ifelse(dat$tactic=='call',1,0)
dat <- dat[!is.na(dat$Mean_body_length_male.mm.), ]

mod <- lm(tactic_binary~Mean_body_length_male.mm., data = dat)
summary(mod)


dat$predicted <- predict(mod, type = "response")
my_plot <- dat%>%
  ggplot(aes(x=Mean_body_length_male.mm.,y=tactic_binary)) +
  xlab('Body size (mm)') +
  #geom_boxplot() +
  ylab('Calling probability') +
  geom_jitter(size=5,width=0.2,height=0.01,alpha=0.8) +
  geom_line(aes(y=predicted),size=2) +
  #geom_errorbar(aes(ymin=ci_min, ymax=ci_max), width=0.3,size=1,col='blue') +
  theme_classic(base_size = 25)
ggsave("./plots/figS9.png", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)


## Fig. S10: Plot for Body Size comparison between bafflers and callers ####
# import data
body_size <- read.csv("./suppl_body_size_baffler.csv", stringsAsFactors = T)

# plot the box and whisker plots
plot_body_size <- ggplot(body_size, aes(x = Tactic, y = Body_Length_mm, fill = Tactic)) +
  geom_boxplot(alpha = 0.85) +
  geom_jitter(width = 0.1, alpha = 0.5, size = 1.5) +
  scale_fill_manual(values = c("#E69F00","#56B4E9")) +
  labs(fill = "",
       x = "Tactic",
       y = "Body Size (mm)"
  ) +
  theme_minimal()+
  theme(axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

lm.bodysize <- lm(Body_Length_mm~Tactic , data = body_size)

ggsave("./plots/figS10.png", plot = plot_body_size , width = 10, height = 7)


## Fig. S11: Plot for Baffling Propensity as a function of body size  ####
# import data
baff_prop <- read.csv("./suppl_baffling_propensity.csv", stringsAsFactors = T)

baff_prop <- baff_prop %>% 
  mutate(baff_probability = ifelse(Tactic == "Baffling", 1, 0))

# plot a scatter plot with a linear fit
plot_baff_prop <- ggplot(baff_prop, aes(x = Body_Length_mm, y = baff_probability)) +
  geom_point() +  
  labs(
    title = "",
    x = "Body Size (mm)",
    y = "Baffling Probability"
  ) +
  theme_minimal()+
  theme(axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  geom_smooth(method = "lm", color = "blue", size = 1, se = FALSE)

lm.baffprop <- lm(baff_probability~Body_Length_mm, data = baff_prop)

ggsave("./plots/figS11.png", plot = plot_baff_prop , width = 10, height = 7)

### Exporting output as .CSV files ####

# Changing column names for exporting the .CSV file with confidence intervals
colnames(within_bush_bootstrap_CI) <- c(
  "Experiment",         
  "Treatment",                
  "Female_Mating_Status",   
  "Strategy",        
  "Mean",           
  "Lower_95_CI",            
  "Upper_95_CI"             
)

# Write data to .csv file
write.csv(within_bush_bootstrap_CI, "./output/confidence_intervals.csv",append = FALSE)










