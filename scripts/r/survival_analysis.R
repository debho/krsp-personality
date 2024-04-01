# MAIN FILE
# runs models once i have all the data cleaned and consolidated

# Analysis of survival
options(tidyverse.quiet = TRUE)
library(tidyverse)
library(tidylog)
library(lme4)
library(car)
library(Matrix)
library(DHARMa)
library(standardize)
library(ade4)
library(lubridate)
library(performance)

# scale growth rate and part dates by grid-year combination
personality = read_csv('data/personality-mrw-survival.csv', show_col_types = FALSE) %>% 
  mutate(survival = as.integer(survived_200d)) %>% 
  group_by(grid, year) %>% 
  mutate(growth_sc = scale(growth, scale = T, center = T)[,1],
         part_sc = scale(part, scale = T, center = T)[,1]) %>% 
  ungroup() 

# PCA on raw OFT/MIS data ####
personality[is.na(personality$oft_duration),
            "oft_duration"] <- 450.000
personality[is.na(personality$mis_duration),
            "mis_duration"] <- 300.000

aggression = personality %>% 
  select(front, back, attack, attacklatency, approachlatency, mis_duration) %>% 
  mutate(across(everything(), ~if_else(is.na(.x), median(.x, na.rm = TRUE), .x))) %>%
  mutate(front = front/mis_duration,
         back = back/mis_duration,
         attack = attack/mis_duration,
         attacklatency = attacklatency/mis_duration,
         approachlatency = approachlatency/mis_duration) %>%
  select(-mis_duration)

pca_agg = prcomp(aggression, scale = TRUE, center = TRUE)
mis1 = predict(pca_agg)[,1]

activity = personality %>% 
  select(walk, still, hang, jump, chew, hole, groom, oft_duration) %>% 
  mutate(across(everything(), ~if_else(is.na(.x), median(.x, na.rm = TRUE), .x)))%>%
  mutate(walk = walk/oft_duration,
         still = still/oft_duration,
         hang = hang/oft_duration,
         jump = jump/oft_duration,
         chew = chew/oft_duration,
         hole = hole/oft_duration,
         groom = groom/oft_duration) %>%
  select(-oft_duration)

pca_act = prcomp(activity, scale = TRUE, center = TRUE)
oft1 = predict(pca_act)[,1]

personality$oft1 = unlist(oft1 * -1)
personality$mis1 = unlist(mis1 * -1)


# scale personality by gridyear 
personality <- personality %>%
  mutate(grid_density = scale(grid_density, scale = T, center = T)[,1],
         age_sc = scale(age_at_trial, scale = T, center = T)[,1]) %>%
  group_by(grid, year) %>%
  mutate(oft1 = scale(oft1, scale = T, center = T)[,1],
         mis1 = scale(mis1, scale = T, center = T)[,1]) %>%
  ungroup() %>%
  mutate(treatment = factor(treatment))

personality_imputed <- personality %>%
  group_by(grid, year) %>%
  mutate(growth = ifelse(is.na(growth),
                            mean(growth, na.rm = T),
                            growth)) %>%
  mutate(growth_sc = scale(growth, scale = T, center = T)[,1]) %>%
  ungroup()
  

# Factors that may influence personality #### ----------------------------------
oft_indiv <- lmer(oft1 ~
                    sex + 
                    age_sc*grid_density +
                    growth_sc*grid_density +
                    part_sc*grid_density +
                    mastyear +
                    treatment +
                    (1|year) +
                    (1|litter_id),
                  data = dat)
summary(oft_indiv) #no significant effects

mis_indiv <- lmer(mis1 ~
                    sex +
                    age_sc*grid_density +
                    growth_sc*grid_density +
                    part_sc*grid_density +
                    mastyear +
                    treatment +
                    (1|year) +
                    (1|litter_id),
                  data = dat)
summary(mis_indiv) #no significant effects

# n=1 missing oft1
# n=1 missing mis1
# n=35 missing growth_sc
# n=3 missing part_sc

# Model 1 Survival to autumn #### -------------------------------------------------
dat = personality_imputed %>% 
  mutate(across(c(year, dam_id, litter_id, grid, mastyear), as_factor)) 

survival_to_autumn = glmer(made_it ~
                             oft1*mis1*grid_density +
                             part_sc*grid_density +
                             growth_sc*grid_density +
                             mastyear +
                             treatment +
                             (1|year) +
                             (1|litter_id), 
                           data = dat,
                           na.action = 'na.omit',
                           family = binomial(link = "logit"),
                           control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))

car::Anova(survival_to_autumn)
summary(survival_to_autumn)
confint(survival_to_autumn,
        method = "Wald") # 95%CIs

# Diagnostics
simulationOutput = simulateResiduals(survival_to_autumn, plot = F)

residuals(simulationOutput, quantileFunction = qnorm)
plot(simulationOutput)
# Looks pretty good

# Model 2 Survival to 200 days #### --------------------------------------------
dat2 = personality_imputed %>% 
  mutate(across(c(year, dam_id, litter_id, grid, mastyear), as_factor))

survival_to_200d = glmer(survived_200d ~ 
                           oft1*mis1*grid_density +
                           part_sc*grid_density +
                           growth_sc*grid_density +
                           mastyear +
                           treatment +
                           (1|year) +
                           (1|litter_id),
                         data = dat2,
                         na.action = 'na.omit',
                         family = 'binomial'(link = "logit"),
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))
#allFit(mod)
car::Anova(survival_to_200d)
summary(survival_to_200d)
confint(survival_to_200d,
        method = "Wald") #95% CIs

# Diagnostics
simulationOutput = simulateResiduals(survival_to_200d, plot = F)

residuals(simulationOutput, quantileFunction = qnorm)
plot(simulationOutput)
# Theres some wonkiness here with one of the random effects deviating
# But it seems to not be a massive issue

icc(survival_to_autumn, by_group = T, tolerance = 0)
icc(survival_to_200d, by_group = T, tolerance = 0)
vif(survival_to_autumn)
vif(survival_to_200d)

