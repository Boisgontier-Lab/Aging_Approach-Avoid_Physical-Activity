# load packages -----
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(ggbeeswarm)
library(psych)
library(dplyr)
library(lmerTest)
library(languageR)
library(effects)
library(ggplot2)
library(MuMIn)
library(lattice)
library(splithalf)

# read in data ------
data1 <- read.csv(here("data_lifespan", "2022-08-23_manikin_final.csv"))

# tidying data  ------
# Renaming factors -----
names(data1)
cleandata1 <- data1 %>%
  clean_names() %>%
  rename(chronic01 = chronic19_none) %>%
  rename(exptime = total_elapsed_time) %>%
  rename(down = responsekey_down) %>%
  rename(language = subj) %>%
  rename(up = responsekey_up) %>%
  rename(daytime = time)
names(cleandata1)

# adding column with trial number of the whole study
cleandata1 <- cleandata1 %>%
  group_by(id) %>%
  mutate(trial_number_study = 1:n()) %>%
  ungroup()

# adding column with mean attitude (attitude1 + attitude2)/2
cleandata1 <- cleandata1 %>%
  mutate(attitude = (attitude1 + attitude2)/2)

# calculate bmi
cleandata1 <- cleandata1 %>%
  mutate(height_m = (height/100))
cleandata1 <- cleandata1 %>%
  mutate(bmi = (weight/I(height_m^2)))

# excluding participants and removing trials not related to performance or related to familiarization ------
#unique (cleandata1$age)
cleanlightdata1 <-
    cleandata1 %>%
    filter(!(trialcode %in% c("fixation", "reminder", "too_slow",
                              "instructionimages", "error"))) %>%
    filter(!(computer_platform %in% c("ios", "and"))) %>%
    filter(latency < 3000 & latency > 150) %>%
    filter(height < 250) %>%
    filter(weight > 30 & weight < 200) %>%
    filter(!(id %in% c("COWIB7NFR", "STMAG0RFR", "CHMAM5AEN",
                       "PHGU70EFR", "Vimi00elFR", "Joro00rf", "HEMAUDFR"))) %>%
    filter(!(trial_num %in% c("1", "2", "3", "4", "5", "6"))) %>%
    filter(!(trial_number_study %in% c(as.character(7:30))))


# Renaming block numbers -----
cleanlightdata1 <-
    cleanlightdata1 %>%
    mutate(block_num = recode_factor(block_num, `4` = "1", `7` = "2",
                                     `10` = "3", `12` = "4"))

# Creating new variables --------
cleanlightdata1 <-
    cleanlightdata1 %>%
    mutate(manikintop = as.integer(str_ends(trialcode, "_ManikinBottom"))) %>%
    mutate(approach = as.integer(str_detect(trialcode, "Approach"))) %>%
    mutate(geomfigure = as.integer(str_starts(trialcode, "square") |
                                   str_starts(trialcode, "circle")))

# Make stimulus variable from trialcode
cleanlightdata1 <-
    cleanlightdata1 %>%
    mutate(stimulus = recode(trialcode, 
                             "circleAvoid_ManikinBottom" = "circle",
                             "squareApproach_ManikinBottom" = "square",
                             "squareApproach_ManikinTop" = "square",
                             "circleAvoid_ManikinTop" = "circle",
                             "ApAvoid_ManikinBottom" = "ap",
                             "SedenApproach_ManikinBottom" = "sed",
                             "ApAvoid_ManikinTop" = "ap",
                             "SedenApproach_ManikinTop" = "sed",
                             "ApApproach_ManikinTop" = "ap",
                             "ApApproach_ManikinBottom" = "ap",
                             "SedenAvoid_ManikinBottom" = "sed",
                             "SedenAvoid_ManikinTop" = "sed",
                             "squareAvoid_ManikinTop" = "square",
                             "squareAvoid_ManikinBottom" = "square",
                             "circleApproach_ManikinBottom" = "circle",
                             "circleApproach_ManikinTop" = "circle"
                             ))

# Reverse coding approach/avoid (=1, vs avoid=0)
cleanlightdata1 <-
    cleanlightdata1 %>%
    mutate(avoid.1.approach.0 = recode(approach, `1` = 0L, `0` = 1L))

# reversing coding of correct trial = 1 and error = 0 to correct trial = 0 and error = 1
cleanlightdata1 <-
    cleanlightdata1 %>%
    mutate(error = recode(correct, `1` = 0L, `0` = 1L))

# Creating binary variable for sex
cleanlightdata1 <-
    cleanlightdata1 %>%
    mutate(sex01 = case_when(sex == "Male" ~ 1L,
                             sex == "Mâle" ~ 1L,
                             sex == "Female" ~ 0L,
                             sex == "Femelle" ~ 0L))

# Creating binary intention
cleanlightdata1 <-
    cleanlightdata1 %>%
    mutate(intention01 = recode(intention,
                                `1` = 0L, `2` = 0L, `3` = 0L, `4`= 0L,
                                `5` = 0L, `6` = 0L, `7` = 1L))

# Reverse coding intention
cleanlightdata1 <-
    cleanlightdata1 %>%
    mutate(intention01.reversed = recode(intention01,
                                         `1` = 0L, `0` = 1L))

# Creating binary attitude
cleanlightdata1 <-
    cleanlightdata1 %>%
    mutate(attitude01 = if_else(attitude < 7, 0, 1))

# Reverse coding intention
cleanlightdata1 <-
    cleanlightdata1 %>%
    mutate(attitude01.reversed = recode(attitude01, `1` = 0L, `0` = 1L))

# creating a columns of the different pictograms appearing on the screen for each trial
cleanlightdata1 <- cleanlightdata1 %>%
  mutate(pictograms = case_when(stimulus=="square" ~ square_pic, 
                                stimulus=="circle" ~ circle_pic,
                                stimulus=="ap" ~ ap_pic, 
                                TRUE ~ sed_pic))

############### Per-subject means (reaction time)/counts (errors)
cleanlightdata1_mean_by_cond <-
  cleanlightdata1 %>%
  mutate(trialcode_no_top_bot = gsub("(.*)(Bottom|Top)", "\\1", trialcode)) %>%
  group_by(id, trialcode_no_top_bot) %>%
  summarise(mean = mean(latency, na.rm = TRUE),
            num_error = sum(error, na.rm = TRUE),
            error_ratio = mean(error, na.rm = TRUE),
            group_size = n(), .groups = "keep") %>%
  ungroup() %>%
  pivot_wider(id_cols = id, names_from = trialcode_no_top_bot,
              values_from = c(mean, num_error, group_size, error_ratio)) %>%
  group_by(id) %>%
  mutate(mean_geomAvoid = weighted.mean(c(mean_circleAvoid_Manikin,
                                          mean_squareAvoid_Manikin),
                                        w = c(group_size_circleAvoid_Manikin,
                                              group_size_squareAvoid_Manikin),
                                        na.rm = TRUE),
         mean_geomApproach = weighted.mean(c(mean_circleApproach_Manikin,
                                             mean_squareApproach_Manikin),
                                           w = c(group_size_circleApproach_Manikin,
                                                 group_size_squareApproach_Manikin),
                                           na.rm = TRUE),
         sum_error_geomAvoid = sum(num_error_circleAvoid_Manikin,
                                   num_error_squareAvoid_Manikin, na.rm = TRUE),
         sum_error_geomApproach = sum(num_error_circleApproach_Manikin,
                                      num_error_squareApproach_Manikin, na.rm = TRUE),
         error_ratio_geomAvoid = sum_error_geomAvoid /
           sum(group_size_circleAvoid_Manikin,
               group_size_squareAvoid_Manikin, na.rm = TRUE),
         error_ratio_geomApproach = sum_error_geomApproach /
           sum(group_size_circleApproach_Manikin,
               group_size_squareApproach_Manikin, na.rm = TRUE)) %>%
  ungroup() %>%
  select(!starts_with("group_size")) %>%
  mutate(diff_ApAvoid = mean_ApAvoid_Manikin - mean_geomAvoid,
         diff_SedenAvoid = mean_SedenAvoid_Manikin - mean_geomAvoid,
         diff_ApApproach = mean_ApApproach_Manikin - mean_geomApproach,
         diff_SedenApproach = mean_SedenApproach_Manikin - mean_geomApproach)

cleanlightdata1 <- left_join(cleanlightdata1, cleanlightdata1_mean_by_cond, by = "id")

cleanlightdata1_error_approach_avoid <-
  cleanlightdata1_mean_by_cond %>%
  select(id, sum_error_geomAvoid, sum_error_geomApproach,
         error_ratio_geomAvoid, error_ratio_geomApproach) %>%
  pivot_longer(!id,
               names_to = c(".value", "approach_or_avoid"),
               names_pattern = "(.*)(Avoid|Approach)$") %>%
  mutate(approach_or_avoid = tolower(approach_or_avoid))

cleanlightdata1 <- cleanlightdata1 %>%
  mutate(approach_or_avoid =
           case_when(
             str_starts(block_code, "approach_") ~ "approach",
             str_starts(block_code, "avoid_") ~ "avoid",
             TRUE ~ NA_character_
           ))

cleanlightdata1 <- left_join(cleanlightdata1, cleanlightdata1_error_approach_avoid,
                             by = c("id", "approach_or_avoid"))

## Adding columns with means per subject -----
# per level of the factors stimulus + approach -----
# and per level of geomfigure (1,0) -----
cleanlightmeansdata1 <- cleanlightdata1 %>%
  group_by(id, stimulus, approach) %>%
  mutate(mean_latency = mean(latency, na.rm = TRUE)) %>% 
  mutate(mean_error = mean(error, na.rm = TRUE)) %>% 
  ungroup() %>%
  
  group_by(id, geomfigure) %>%
  mutate(mean_latency_geom = mean(latency, na.rm = TRUE)) %>%
  mutate(mean_error_geom = mean(error, na.rm = TRUE)) %>%
  ungroup() %>%
  
  group_by(id, approach, geomfigure) %>%
  mutate(mean_geom_direction = mean(latency, na.rm = TRUE)) %>% 
  mutate(mean_error_geom_direction = mean(error, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  group_by(id, geomfigure) %>%
  mutate(mean_only_geom = mean(latency, na.rm = TRUE)) %>% 
  mutate(mean_error_only_geom = mean(error, na.rm = TRUE)) %>% 
  ungroup() 

# adding column with trial number of the whole study
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  group_by(id) %>%
  mutate(trial_number_study = 1:n()) %>%
  ungroup()

# Adding column with mean for geomfigure for each subject
# Use fill from tidyr after changing the 'mean_only_geom' values to NA that corresponds to 0 in 'geomfigure'
# By default, the .direction (argument in fill) is "down", but it can also take "up" (mean_only_geom, .direction="up")
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(mean_only_geom = NA^(!geomfigure)*mean_only_geom) %>%
  fill(mean_only_geom)

cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(mean_error_only_geom = NA^(!geomfigure)*mean_error_only_geom) %>%
  fill(mean_error_only_geom)

# Adding column with mean for geomfigure and direction for each subject
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(mean_geom_direction = NA^(!geomfigure)*mean_geom_direction) %>%
  group_by(id, approach) %>%
  mutate(mean_geom_direction = mean(mean_geom_direction, na.rm = TRUE)) 

cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(mean_error_geom_direction = NA^(!geomfigure)*mean_error_geom_direction) %>%
  group_by(id, approach) %>%
  mutate(mean_error_geom_direction = mean(mean_error_geom_direction, na.rm = TRUE)) 

## Change variable class
cleanlightmeansdata1 <-
    cleanlightmeansdata1 %>%
    mutate(manikintop = as.factor(manikintop),
           approach = as.factor(approach),
           stimulus = as.factor(stimulus),
           trialcode = as.factor(trialcode),
           geomfigure = as.factor(geomfigure),
           id = as.factor(id),
           age = as.numeric(age),
           sex01 = as.factor(sex01),
           pictograms = as.character(pictograms),
           chronic01 = as.factor(chronic01),
           intention01 = as.factor(intention01))

# Adding column latency minus the mean latency for geometrical figures 
# irrespective of the type of figure (circle, square) and the type of movement (approach, avoid)
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(relativelatencygeom = latency - mean_only_geom)

# irrespective of the type of figure (circle, square)
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(relativelatencygeomdirection = latency - mean_geom_direction)

# creating column with means of relative latency
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  group_by(id, geomfigure, approach) %>%
  mutate(relativelatency = mean(relativelatencygeomdirection, na.rm = TRUE)) %>% 
  ungroup() 

# creating column with number of minutes per week in moderate and vigorous physical activity (MVPA)
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(mvpa = (moderate_d * moderate_m + vigorous_d * vigorous_m))

# creating column with raw ap bias
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(biasapraw = (mean_ApAvoid_Manikin - mean_ApApproach_Manikin))

# creating column with raw sed bias
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(biassedraw = (mean_SedenAvoid_Manikin - mean_SedenApproach_Manikin))


# creating column with corrected ap bias 
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(biasapcorr = (diff_ApAvoid - diff_ApApproach))


# creating column with corrected sed bias
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(biassedcorr = (diff_SedenAvoid - diff_SedenApproach))

# Creating new file -----
data2 <- cleanlightmeansdata1

## Computing internal consistency for each stimulus of the approach avoidance task
## splithalf did not recognized that a participant had only NA and should be removed
## We exclude this "NA participant" with this code
## data2_splithalf <-
##   data2 %>%
##   group_by(id) %>%
##   mutate(has_approach_and_avoid = any(approach_or_avoid %in% "avoid") &
##              any(approach_or_avoid %in% "approach")) %>%
##   ungroup() %>%
##   filter(has_approach_and_avoid == TRUE)

## Make a new condition variable grouping together square and circle as neutral
data2_splithalf <-
    data2 %>%
    mutate(stim_3_cat = case_when(stimulus == "square" ~ "neutral",
                                  stimulus == "circle" ~ "neutral",
                                  stimulus == "ap" ~ "ap",
                                  stimulus == "sed" ~ "sed"))

## Splithalf reaction time
int_consist_rt_by_cond <-
    splithalf(data = data2_splithalf, outcome = "RT",
              var.RT = "latency", var.participant = "id",
              var.compare = "approach_or_avoid",
              compare1 = "approach", compare2 = "avoid",
              var.condition = "stim_3_cat",
              conditionlist = c("ap", "sed", "neutral"))

int_consist_rt_all <-
    splithalf(data = data2_splithalf, outcome = "RT",
              var.RT = "latency", var.participant = "id",
              var.compare = "approach_or_avoid",
              compare1 = "approach", compare2 = "avoid")

## Splithalf errors
int_consist_err_by_cond <-
    splithalf(data = data2_splithalf, outcome = "accuracy",
              var.ACC = "correct", var.participant = "id",
              var.compare = "approach_or_avoid",
              compare1 = "approach", compare2 = "avoid",
              var.condition = "stim_3_cat",
              conditionlist = c("ap", "sed", "neutral"))

int_consist_err_all <-
    splithalf(data = data2_splithalf, outcome = "accuracy",
              var.ACC = "correct", var.participant = "id",
              var.compare = "approach_or_avoid",
              compare1 = "approach", compare2 = "avoid")

# exploring data -------
#glimpse(data2)
#skim(data2)
#hist(data2$age, breaks=50)
#plot (data2$age)
#describe (data2$age)
#plot (data2$height)
#describe (data2$height)
#plot (data2$weight)
#describe (data2$weight)
#plot (data2$mvpa)
#plot(data2$mvpa~data2$age)
#describe (data2$mvpa)
#plot (data2$bmi)
#describe (data2$bmi)
#plot(data2$bmi~data2$age)

# centering/standardizing  -----
data2 <-
    data2 %>%
    mutate(age_c = scale(age, center = TRUE, scale = TRUE),
           mvpa_c = scale(mvpa, center = TRUE, scale = TRUE),
           bmi_c = scale(bmi, center = TRUE, scale = TRUE),
           diff_ApAvoid_c = scale(diff_ApAvoid, center = TRUE, scale = TRUE),
           diff_SedenAvoid_c = scale(diff_SedenAvoid, center = TRUE, scale = TRUE),
           diff_ApApproach_c = scale(diff_ApApproach, center = TRUE, scale = TRUE),
           diff_SedenApproach_c = scale(diff_SedenApproach, center = TRUE, scale = TRUE))

###### LINEAR MIXED EFFECTS MODELS / HIERARCHICAL MODELS
### Models testing the effect of AGE on reaction time
# interaction age x movement (approach, avoid  in the physical activity condition
lmm1 <- lmer(latency  ~ 1 + age_c*approach + mvpa_c + chronic01 + sex01 + bmi_c + (approach|id), 
             data=data2, subset = stimulus == "ap" & error == 0, REML = TRUE, na.action=na.omit)
summary(lmm1)
#confint(lmm1) #95% confidence interval
plot(allEffects(lmm1), selection=5)
r.squaredGLMM(update(lmm1, REML = FALSE)) # Effect sizes were compared with REML=F
# To test simple effects, replace "approach" with "avoid.1.approach.0"

# interaction age x movement (approach, avoid) in the sedentary condition
lmm2 <- lmer(latency~ 1 + age_c*approach + mvpa_c + chronic01 + sex01 + bmi_c + (approach|id), 
             data=data2, subset = stimulus == "sed" & error == 0, REML = TRUE, na.action=na.omit)
summary(lmm2)
#confint(lmm2) #95% confidence interval
plot(allEffects(lmm2), selection=5)
r.squaredGLMM(update(lmm2, REML = FALSE)) # Effect sizes were compared with REML=F

### Models testing the effect of GEOMETRICAL stimuli on reaction time
# interaction age x movement (approach, avoid)  in the geometric figures
lmm3 <- lmer(latency ~ 1 + age_c*approach + mvpa_c + chronic01 + sex01 + bmi_c +  (approach|id), 
             data=data2, subset = geomfigure == "1" & error == 0, REML = TRUE, na.action=na.omit)
summary(lmm3)
#confint(lmm3) #95% confidence interval
plot(allEffects(lmm3), selection=5)
r.squaredGLMM(update(lmm3, REML = FALSE)) # Effect sizes were compared with REML=F

### Models testing the effect of AGE on CORRECTED reaction time
# interaction age x movement (approach, avoid)  in the physical activity condition
lmm4 <- lmer(relativelatencygeomdirection  ~ 1 + age_c*approach + mvpa_c + chronic01 + sex01 + bmi_c +  (approach|id), 
             data=data2, subset = stimulus == "ap" & error == 0, REML = TRUE, na.action=na.omit)
summary(lmm4)
#confint(lmm4) #95% confidence interval
plot(allEffects(lmm4), selection=5)
r.squaredGLMM(update(lmm4, REML = FALSE)) # Effect sizes were compared with REML=F

# interaction age x movement (approach, avoid) in the sedentary condition
lmm5 <- lmer(relativelatencygeomdirection  ~ 1 + age_c*approach + mvpa_c + chronic01 + sex01 + bmi_c + (approach|id), 
             data=data2, subset = stimulus == "sed" & error == 0, REML = TRUE, na.action=na.omit)
summary(lmm5)
#confint(lmm5) #95% confidence interval
plot(allEffects(lmm5), selection=5)
r.squaredGLMM(update(lmm5, REML = FALSE)) # Effect sizes were compared with REML=F

### Models with DICHOTOMIC attitude and intention
# ATTITUDE x movement (approach, avoid)  in the physical activity condition
lmm6 <- lmer(relativelatencygeomdirection  ~ 1 + attitude01.reversed*avoid.1.approach.0 + intention01*avoid.1.approach.0 
             + age_c + sex01 + bmi_c + mvpa_c + chronic01 + (approach|id), 
                     data=data2, subset = stimulus == "ap"  & error == 0, REML = TRUE, na.action=na.omit)
summary(lmm6)
confint(lmm6) #95% confidence interval
plot(allEffects(lmm6))
plot(allEffects(lmm6), selection=6)
plot(allEffects(lmm6), selection=7)
r.squaredGLMM(lmm6) # Effect sizes were compared with REML=F

# Same model with the "approach" variable being coded the opposite way, to look at the simple effect
lmm6.2 <- lmer(relativelatencygeomdirection  ~ 1 + attitude01*avoid.1.approach.0 + intention01*avoid.1.approach.0 
               + age_c + sex01 + bmi_c + mvpa_c + chronic01 + (approach|id), 
             data=data2, subset = stimulus == "ap"  & error == 0, REML = TRUE, na.action=na.omit)
summary(lmm6.2)
confint(lmm6.2) #95% confidence interval
plot(allEffects(lmm6.2))
r.squaredGLMM(update(lmm6.2, REML = FALSE)) # Effect sizes were compared with REML=F

# ATTITUDE x movement (approach, avoid) in the sedentary condition
lmm7 <- lmer(relativelatencygeomdirection  ~ 1 + attitude01.reversed*avoid.1.approach.0  + intention01*avoid.1.approach.0  
             + age_c + sex01 + bmi_c + mvpa_c + chronic01 + (approach|id), 
                     data=data2, subset = stimulus == "sed" & error == 0, REML = TRUE, na.action=na.omit)
summary(lmm7)
confint(lmm7) #95% confidence interval
plot(allEffects(lmm7))
plot(allEffects(lmm7), selection=6)
plot(allEffects(lmm7), selection=7)
r.squaredGLMM(update(lmm7, REML = FALSE)) # Effect sizes were compared with REML=F

# Same model with the "approach" variable being coded the opposite way, to look at the simple effect
lmm7.2 <- lmer(relativelatencygeomdirection  ~ 1 + attitude01*avoid.1.approach.0  + intention01*avoid.1.approach.0  
               + age_c + sex01 + bmi_c + mvpa_c + chronic01 + (approach|id), 
             data=data2, subset = stimulus == "sed" & error == 0, REML = TRUE, na.action=na.omit)
summary(lmm7.2)
confint(lmm7.2) #95% confidence interval
r.squaredGLMM(update(lmm7.2, REML = FALSE)) # Effect sizes were compared with REML=F

#################################################################
##### Hierarchical model with binomial dependent variable #######
#################################################################

glm1 <- glmer(error ~ 1 + age_c*approach + (approach|id), family="binomial", 
              data=data2, subset = stimulus == "ap", na.action=na.omit)
summary(glm1)
confint(glm1, parm="beta_", method="Wald") #95% confidence interval
plot(allEffects(glm1))
r.squaredGLMM(glm1)

# interaction age x movement (approach, avoid) in the sedentary condition
glm2 <- glmer(error ~ 1 + age_c*approach + (approach|id), family="binomial", 
             data=data2, subset = stimulus == "sed", na.action=na.omit)
summary(glm2)
confint(glm2, parm="beta_", method="Wald") #95% confidence interval
plot(allEffects(glm2))
r.squaredGLMM(glm2) 

### Models testing the effect of GEOMETRICAL stimuli on errors
# interaction age x movement (approach, avoid)  in the geometric figures
glm3 <- glmer(error ~ 1 + age_c*approach + (approach|id), family="binomial", 
             data=data2, subset = geomfigure == "1", na.action=na.omit)
summary(glm3)
confint(glm3, parm="beta_", method="Wald") #95% confidence interval
plot(allEffects(glm3))
r.squaredGLMM(glm3) 

### Models testing the effect of AGE on errors (DV) with the mean number per subject on the approach and avoid condition as independent variable
# interaction age x movement (approach, avoid)  in the physical activity condition
glm4 <- glmer(error ~ age_c*approach + error_ratio_geom + (approach|id), family="binomial", 
             data=data2, subset = stimulus == "ap", na.action=na.omit)
summary(glm4)
confint(glm4, parm="beta_", method="Wald") #95% confidence interval
plot(allEffects(glm4))
plot(allEffects(glm4), selection=2)
r.squaredGLMM(glm4) 

# interaction age x movement (approach, avoid) in the sedentary condition
glm5 <- glmer(error  ~ age_c*approach + error_ratio_geom +  (approach|id), family="binomial",
             data=data2, subset = stimulus == "sed", na.action=na.omit)
summary(glm5)
confint(glm5, parm="beta_", method="Wald") #95% confidence interval
plot(allEffects(glm5), selection=2)
r.squaredGLMM(glm5) 

### Models with DICHOTOMIC attitude and intention
# ATTITUDE x movement (approach, avoid)  in the physical activity condition
glm6attitude <- glmer(error  ~ attitude01.reversed*avoid.1.approach.0 + error_ratio_geom + (approach|id), family="binomial",
             data=data2, subset = stimulus == "ap", na.action=na.omit)
summary(glm6attitude)
confint(glm6attitude, parm="beta_", method="Wald") #95% confidence interval
plot(allEffects(glm6attitude))
plot(allEffects(glm6attitude), selection=2)
r.squaredGLMM(glm6attitude) 

glm6intention <- glmer(error  ~ intention01*avoid.1.approach.0 + error_ratio_geom + (approach|id), family="binomial",
                      data=data2, subset = stimulus == "ap", na.action=na.omit)
summary(glm6intention)
confint(glm6intention, parm="beta_", method="Wald") #95% confidence interval
plot(allEffects(glm6intention))
plot(allEffects(glm6intention), selection=2)
r.squaredGLMM(glm6intention) 

# Same model with the "approach" variable being coded the opposite way, to look at the simple effect
glm6.2attitude <- glmer(error  ~ attitude01*avoid.1.approach.0 + error_ratio_geom + (approach|id), family="binomial", 
               data=data2, subset = stimulus == "ap", na.action=na.omit)
summary(glm6.2attitude)
confint(glm6.2attitude, parm="beta_", method="Wald") #95% confidence interval
plot(allEffects(glm6.2attitude))
r.squaredGLMM(glm6.2attitude) 

# ATTITUDE x movement (approach, avoid) in the sedentary condition
glm7attitude <- glmer(error  ~ attitude01*approach + error_ratio_geom + (approach|id), family="binomial", 
             data=data2, subset = stimulus == "sed", na.action=na.omit)
summary(glm7attitude)
confint(glm7attitude, parm="beta_", method="Wald") #95% confidence interval
plot(allEffects(glm7attitude))
plot(allEffects(glm7attitude), selection=2)
r.squaredGLMM(glm7attitude) 

glm7intention <- glmer(error  ~ intention01*approach + error_ratio_geom + (approach|id), family="binomial", 
                      data=data2, subset = stimulus == "sed", na.action=na.omit)
summary(glm7intention)
confint(glm7intention, parm="beta_", method="Wald") #95% confidence interval
plot(allEffects(glm7intention))
plot(allEffects(glm7intention), selection=2)
r.squaredGLMM(glm7intention) 

# Same model with the "approach" variable being coded the opposite way, to look at the simple effect
glm7.2attitude <- glmer(error  ~ attitude01*approach  + error_ratio_geom + (approach|id), family="binomial",
               data=data2, subset = stimulus == "sed", na.action=na.omit)
summary(glm7.2attitude)
confint(glm7.2attitude, parm="beta_", method="Wald") #95% confidence interval
r.squaredGLMM(glm7.2attitude) 

glm7.2intention <- glmer(error ~ intention01*avoid.1.approach.0 + error_ratio_geom + (approach|id), family="binomial",
                        data=data2, subset = stimulus == "sed", na.action=na.omit)
summary(glm7.2intention)
confint(glm7.2intention, parm="beta_", method="Wald") #95% confidence interval
r.squaredGLMM(glm7.2intention) 
