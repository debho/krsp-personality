# MAIN FILE
# runs models once i have all the data cleaned and consolidated

# Analysis of survival
options(tidyverse.quiet = TRUE)
library(tidyverse)
library(lme4)
library(car)
library(DHARMa)
library(Matrix)
library(standardize)
library(ade4)

personality = read_csv('data/personality-mrw-survival.csv', show_col_types = FALSE) %>% 
  mutate(part = scale(part, center = T),
         age = scale(age_at_trial, center = T),
         grid_density = scale(grid_density, center = T),
         gridyear = as.factor(gridyear),
         sex = as.factor(sex),
         year = as.factor(year),
         cohort = as.factor(cohort),
         grid = as.factor(grid),
         survived_200d = as.integer(survived_200d),
         litter_id = as.factor(litter_id),
         dam_id = as.factor(dam_id)) %>%
  filter(!is.na(walk),
         !is.na(approachlatency))

# GETTING PCA LOADINGS AND OFT/MIS SCORES 
personality[is.na(personality$oft_duration),
            "oft_duration"] <- 450.000
personality[is.na(personality$mis_duration),
            "mis_duration"] <- 300.000

activity <- transmute(personality,
                      walk_prop = (walk/oft_duration),
                      jump_prop = (jump/oft_duration),
                      hole_prop = (hole/oft_duration),
                      hang_prop = (hang/oft_duration),
                      still_prop = (still/oft_duration),
                      chew_prop = (chew/oft_duration),
                      groom_prop = (groom/oft_duration))

pca.oft <- dudi.pca(activity,
                    scale = TRUE,
                    scannf = FALSE,
                    nf = 7)

pca.oft$c1
personality$oft1 <- (pca.oft$l1$RS1 * -1) 
factoextra::get_eig(pca.oft) #PC1 explains 40.02% of the variance

aggression <- transmute(personality,
                        front_prop = (front/mis_duration),
                        back_prop = (back/mis_duration),
                        approachlat_prop = (approachlatency/mis_duration),
                        attacklat_prop = (attacklatency/mis_duration),
                        attack_prop = (attack/mis_duration)) 

pca.mis <- dudi.pca(aggression,
                    scale = TRUE,
                    scannf = FALSE,
                    nf = 5)

pca.mis$c1
personality$mis1 <- pca.mis$l1$RS1
factoextra::get_eig(pca.mis) #PC1 explains 52.71% of the variance

# scale personality and growth rate by gridyear 
personality <- personality %>%
  mutate(oft1 = scale_by(oft1 ~ gridyear),
         mis1 = scale_by(mis1 ~ gridyear),
         growth = scale_by(growth ~ gridyear))

# CHECK FOR THINGS THAT HAVE EFFECTS ON PERSONALITY FIRST (idk wtf i'm doing?????????)
oft_predictors <- lmer(oft1 ~ sex + age + growth + cohort +
                       (1|litter_id) + (1|gridyear),
                       data = personality)
vif(oft_predictors) #all VIF < 3
summary(oft_predictors)

mis_predictors <- lmer(mis1 ~ sex + age + growth + cohort +
                         (1|litter_id) + (1|gridyear),
                       data = personality)
vif(mis_predictors) #all VIF < 3
summary(mis_predictors)

# Survival to fall census -------------------------------------------------
# i need to fix the made_it thing
dat = personality %>% 
  mutate(across(c(year, dam_id, litter_id, grid), as_factor))

survival_to_autumn = glmer(made_it ~ 
                             oft1*mis1*scale(grid_density) + 
                             (1|litter_id), 
                           data = dat,
                           na.action = 'na.omit',
                           family = binomial(link = "logit"),
                           control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))

car::Anova(survival_to_autumn)
summary(survival_to_autumn)

# Diagnostics
simulationOutput = simulateResiduals(survival_to_autumn, plot = F)

residuals(simulationOutput, quantileFunction = qnorm)
plot(simulationOutput)
# Looks pretty good

# Model 2 Survival to 200 days --------------------------------------------
# WHAT FIXED/RANDOM EFFECTS DO I INCLUDE LMAO RIP
survival_to_200d = glmer(survived_200d ~ 
                         oft1 * mis1 * grid_density + growth + age + part +
                         (1|gridyear) + (1|litter_id),
                         data = personality,
                         na.action = 'na.omit',
                         family = 'binomial',
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))
#allFit(mod)
car::Anova(survival_to_200d)
summary(survival_to_200d)

# Diagnostics
simulationOutput = simulateResiduals(survival_to_200d, plot = F)

residuals(simulationOutput, quantileFunction = qnorm)
plot(simulationOutput)
# Theres some wonkiness here with one of the random effects deviating
# But it seems to not be a massive issue
