# runs repeatability estimates on trials 1 and 2 for juvs with at least 2 trials

library(tidyverse)
library(lme4)

#######################
#### MIN. 2 TRIALS ####
#######################

#obtained personality2 by changing trialnumber == 2 in data-cleaning.R to extract all trial 2 juvs
personality2 <- personality2 %>%
  filter(squirrel_id %in% personality$squirrel_id) %>% #removes those missing trial 1
  drop_na(front) #removes those missing MIS

personality_repeat <- personality %>%
  filter(squirrel_id %in% personality2$squirrel_id) %>%
  bind_rows(personality2)

personality_repeat <- personality_repeat %>%
  mutate(gridyear = paste(grid,year))

n_distinct(personality_repeat$squirrel_id)
#n = 60 juveniles

# run PCA on this subset for sanity purposes
personality_repeat[is.na(personality_repeat$oft_duration),
            "oft_duration"] <- 450.000
personality_repeat[is.na(personality_repeat$mis_duration),
            "mis_duration"] <- 300.000

aggression = personality_repeat %>% 
  select(front, back, attack, attacklatency, approachlatency, mis_duration) %>% 
  mutate(across(everything(), ~if_else(is.na(.x), median(.x, na.rm = TRUE), .x))) %>%
  mutate(front = front/mis_duration,
         back = back/mis_duration,
         attack = attack/mis_duration,
         attacklatency = attacklatency/mis_duration,
         approachlatency = approachlatency/mis_duration) %>%
  select(-mis_duration)

pca_agg_rep = prcomp(aggression, scale = TRUE, center = TRUE)
mis1_rep = predict(pca_agg_rep)[,1]

activity = personality_repeat %>% 
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

pca_act_rep = prcomp(activity, scale = TRUE, center = TRUE)
oft1_rep = predict(pca_act_rep)[,1]

personality_repeat$oft1 = unlist(oft1_rep * -1)
personality_repeat$mis1 = unlist(mis1_rep * -1)

#save this dataframe to csv file
write_csv(personality_repeat, "data/personality-repeatability.csv")

personality_repeat <- read.csv("data/personality-repeatability.csv",
                               header = T)
###############
#### OFT 1 ####
###############

#non-adjusted repeatability ####
OFTna_all <- lmer(oft1 ~ (1|squirrel_id) + (1|gridyear),
               personality_repeat)
summary(OFTna_all)
plot(OFTna_all)
hist(resid(OFTna_all))

OFTna_all.sim <- arm::sim(OFTna_all, 1000)
OFTna_all.fixef = OFTna_all.sim@fixef
OFTna_all.ranef = OFTna_all.sim@ranef
OFTna_all.fixef = coda::as.mcmc(OFTna_all.fixef)
MCMCglmm::posterior.mode(OFTna_all.fixef)
coda::HPDinterval(OFTna_all.fixef)

##among-indiv variance
OFTna_all.bID <- OFTna_all.sim@ranef$squirrel_id
OFTna_all.bvar <- as.vector(apply(OFTna_all.bID, 1, var))
OFTna_all.bvar <- coda::as.mcmc(OFTna_all.bvar)
MCMCglmm::posterior.mode(OFTna_all.bvar)
coda::HPDinterval(OFTna_all.bvar)

##residual variance
OFTna_all.rvar <- OFTna_all.sim@sigma^2
OFTna_all.rvar <- coda::as.mcmc(OFTna_all.rvar)
MCMCglmm::posterior.mode(OFTna_all.rvar)
coda::HPDinterval(OFTna_all.rvar)

##repeatability
OFTna_all.rID <- OFTna_all.bvar/(OFTna_all.bvar + OFTna_all.rvar)
MCMCglmm::posterior.mode(OFTna_all.rID)
coda::HPDinterval(OFTna_all.rID)

#adjusted repeatability ####
OFTa <- lmer(oft1 ~ trialnumber + sex +
                   (1|squirrel_id) + (1|gridyear),
                  personality_repeat)
summary(OFTa)
plot(OFTa)
hist(resid(OFTa))

OFTa.sim <- arm::sim(OFTa, 1000)
OFTa.fixef = OFTa.sim@fixef
OFTa.ranef = OFTa.sim@ranef
OFTa.fixef = coda::as.mcmc(OFTa.fixef)
MCMCglmm::posterior.mode(OFTa.fixef)
coda::HPDinterval(OFTa.fixef)

##among-indiv variance
OFTa.bID <- OFTa.sim@ranef$squirrel_id
OFTa.bvar <- as.vector(apply(OFTa.bID, 1, var))
OFTa.bvar <- coda::as.mcmc(OFTa.bvar)
MCMCglmm::posterior.mode(OFTa.bvar)
coda::HPDinterval(OFTa.bvar)

##residual variance
OFTa.rvar <- OFTa.sim@sigma^2
OFTa.rvar <- coda::as.mcmc(OFTa.rvar)
MCMCglmm::posterior.mode(OFTa.rvar)
coda::HPDinterval(OFTa.rvar)

##repeatability
OFTa.rID <- OFTa.bvar/(OFTa.bvar + OFTa.rvar)
MCMCglmm::posterior.mode(OFTa.rID)
coda::HPDinterval(OFTa.rID)

###############
#### MIS 1 ####
###############

#non-adjusted repeatability

MISna <- lmer(mis1 ~ (1|squirrel_id) + (1|gridyear),
                  personality_repeat)
summary(MISna)
plot(MISna)
hist(resid(MISna))

MISna.sim <- arm::sim(MISna, 1000)
MISna.fixef = MISna.sim@fixef
MISna.ranef = MISna.sim@ranef
MISna.fixef = coda::as.mcmc(MISna.fixef)
MCMCglmm::posterior.mode(MISna.fixef)
coda::HPDinterval(MISna.fixef)

##among-indiv variance
MISna.bID <- MISna.sim@ranef$squirrel_id
MISna.bvar <- as.vector(apply(MISna.bID, 1, var))
MISna.bvar <- coda::as.mcmc(MISna.bvar)
MCMCglmm::posterior.mode(MISna.bvar)
coda::HPDinterval(MISna.bvar)

##residual variance
MISna.rvar <- MISna.sim@sigma^2
MISna.rvar <- coda::as.mcmc(MISna.rvar)
MCMCglmm::posterior.mode(MISna.rvar)
coda::HPDinterval(MISna.rvar)

##repeatability
MISna.rID <- MISna.bvar/(MISna.bvar + MISna.rvar)
MCMCglmm::posterior.mode(MISna.rID)
coda::HPDinterval(MISna.rID)

#adjusted repeatability

MISa <- lmer(mis1 ~ trialnumber + sex +
                   (1|squirrel_id) + (1|gridyear),
                 personality_repeat)
summary(MISa)
plot(MISa)
hist(resid(MISa))

MISa.sim <- arm::sim(MISa, 1000)
MISa.fixef = MISa.sim@fixef
MISa.ranef = MISa.sim@ranef
MISa.fixef = coda::as.mcmc(MISa.fixef)
MCMCglmm::posterior.mode(MISa.fixef)
coda::HPDinterval(MISa.fixef)

##among-indiv variance
MISa.bID <- MISa.sim@ranef$squirrel_id
MISa.bvar <- as.vector(apply(MISa.bID, 1, var))
MISa.bvar <- coda::as.mcmc(MISa.bvar)
MCMCglmm::posterior.mode(MISa.bvar)
coda::HPDinterval(MISa.bvar)

##residual variance
MISa.rvar <- MISa.sim@sigma^2
MISa.rvar <- coda::as.mcmc(MISa.rvar)
MCMCglmm::posterior.mode(MISa.rvar)
coda::HPDinterval(MISa.rvar)

##repeatability
MISa.rID <- MISa.bvar/(MISa.bvar + MISa.rvar)
MCMCglmm::posterior.mode(MISa.rID)
coda::HPDinterval(MISa.rID)


####################
#### ALL TRIALS ####
####################

#obtained personality_all by removing trial number filter in data-cleaning.R
personality_all <- personality_all %>%
  drop_na(front,
          walk) #removes all missing behavioral assays

write_csv(personality_all, "data/personality-all.csv")

personality_all <- read.csv("data/personality-all.csv",
                            header = T) %>%
  mutate(gridyear = paste(grid,year))

n_distinct(personality_all$squirrel_id)
#n = 270 juveniles

# run PCA on this for sanity purposes
personality_all[is.na(personality_all$oft_duration),
                   "oft_duration"] <- 450.000
personality_all[is.na(personality_all$mis_duration),
                   "mis_duration"] <- 300.000

aggression = personality_all %>% 
  select(front, back, attack, attacklatency, approachlatency, mis_duration) %>% 
  mutate(across(everything(), ~if_else(is.na(.x), median(.x, na.rm = TRUE), .x))) %>%
  mutate(front = front/mis_duration,
         back = back/mis_duration,
         attack = attack/mis_duration,
         attacklatency = attacklatency/mis_duration,
         approachlatency = approachlatency/mis_duration) %>%
  select(-mis_duration)

pca_agg_all = prcomp(aggression, scale = TRUE, center = TRUE)
mis1_all = predict(pca_agg_all)[,1]

activity = personality_all %>% 
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

pca_act_all = prcomp(activity, scale = TRUE, center = TRUE)
oft1_all = predict(pca_act_all)[,1]

personality_all$oft1 = unlist(oft1_all * -1)
personality_all$mis1 = unlist(mis1_all * -1)

###############
#### OFT 1 ####
###############

#non-adjusted repeatability ####
OFTna_all <- lmer(oft1 ~ (1|squirrel_id) + (1|gridyear),
              personality_all)
summary(OFTna_all)
plot(OFTna_all)
hist(resid(OFTna_all))

OFTna_all.sim <- arm::sim(OFTna_all, 1000)
OFTna_all.fixef = OFTna_all.sim@fixef
OFTna_all.ranef = OFTna_all.sim@ranef
OFTna_all.fixef = coda::as.mcmc(OFTna_all.fixef)
MCMCglmm::posterior.mode(OFTna_all.fixef)
coda::HPDinterval(OFTna_all.fixef)

##among-indiv variance
OFTna_all.bID <- OFTna_all.sim@ranef$squirrel_id
OFTna_all.bvar <- as.vector(apply(OFTna_all.bID, 1, var))
OFTna_all.bvar <- coda::as.mcmc(OFTna_all.bvar)
MCMCglmm::posterior.mode(OFTna_all.bvar)
coda::HPDinterval(OFTna_all.bvar)

##residual variance
OFTna_all.rvar <- OFTna_all.sim@sigma^2
OFTna_all.rvar <- coda::as.mcmc(OFTna_all.rvar)
MCMCglmm::posterior.mode(OFTna_all.rvar)
coda::HPDinterval(OFTna_all.rvar)

##repeatability
OFTna_all.rID <- OFTna_all.bvar/(OFTna_all.bvar + OFTna_all.rvar)
MCMCglmm::posterior.mode(OFTna_all.rID)
coda::HPDinterval(OFTna_all.rID)

#adjusted repeatability ####
OFTa_all <- lmer(oft1 ~ trialnumber + sex +
               (1|squirrel_id) + (1|gridyear),
             personality_all)
summary(OFTa_all)
plot(OFTa_all)
hist(resid(OFTa_all))

OFTa_all.sim <- arm::sim(OFTa_all, 1000)
OFTa_all.fixef = OFTa_all.sim@fixef
OFTa_all.ranef = OFTa_all.sim@ranef
OFTa_all.fixef = coda::as.mcmc(OFTa_all.fixef)
MCMCglmm::posterior.mode(OFTa_all.fixef)
coda::HPDinterval(OFTa_all.fixef)

##among-indiv variance
OFTa_all.bID <- OFTa_all.sim@ranef$squirrel_id
OFTa_all.bvar <- as.vector(apply(OFTa_all.bID, 1, var))
OFTa_all.bvar <- coda::as.mcmc(OFTa_all.bvar)
MCMCglmm::posterior.mode(OFTa_all.bvar)
coda::HPDinterval(OFTa_all.bvar)

##residual variance
OFTa_all.rvar <- OFTa_all.sim@sigma^2
OFTa_all.rvar <- coda::as.mcmc(OFTa_all.rvar)
MCMCglmm::posterior.mode(OFTa_all.rvar)
coda::HPDinterval(OFTa_all.rvar)

##repeatability
OFTa_all.rID <- OFTa_all.bvar/(OFTa_all.bvar + OFTa_all.rvar)
MCMCglmm::posterior.mode(OFTa_all.rID)
coda::HPDinterval(OFTa_all.rID)

###############
#### MIS 1 ####
###############

#non-adjusted repeatability ####
MISna_all <- lmer(mis1 ~ (1|squirrel_id) + (1|gridyear),
                  personality_all)
summary(MISna_all)
plot(MISna_all)
hist(resid(MISna_all))

MISna_all.sim <- arm::sim(MISna_all, 1000)
MISna_all.fixef = MISna_all.sim@fixef
MISna_all.ranef = MISna_all.sim@ranef
MISna_all.fixef = coda::as.mcmc(MISna_all.fixef)
MCMCglmm::posterior.mode(MISna_all.fixef)
coda::HPDinterval(MISna_all.fixef)

##among-indiv variance
MISna_all.bID <- MISna_all.sim@ranef$squirrel_id
MISna_all.bvar <- as.vector(apply(MISna_all.bID, 1, var))
MISna_all.bvar <- coda::as.mcmc(MISna_all.bvar)
MCMCglmm::posterior.mode(MISna_all.bvar)
coda::HPDinterval(MISna_all.bvar)

##residual variance
MISna_all.rvar <- MISna_all.sim@sigma^2
MISna_all.rvar <- coda::as.mcmc(MISna_all.rvar)
MCMCglmm::posterior.mode(MISna_all.rvar)
coda::HPDinterval(MISna_all.rvar)

##repeatability
MISna_all.rID <- MISna_all.bvar/(MISna_all.bvar + MISna_all.rvar)
MCMCglmm::posterior.mode(MISna_all.rID)
coda::HPDinterval(MISna_all.rID)

#adjusted repeatability ####
MISa_all <- lmer(mis1 ~ trialnumber + sex +
                   (1|squirrel_id) + (1|gridyear),
                 personality_all)
summary(MISa_all)
plot(MISa_all)
hist(resid(MISa_all))

MISa_all.sim <- arm::sim(MISa_all, 1000)
MISa_all.fixef = MISa_all.sim@fixef
MISa_all.ranef = MISa_all.sim@ranef
MISa_all.fixef = coda::as.mcmc(MISa_all.fixef)
MCMCglmm::posterior.mode(MISa_all.fixef)
coda::HPDinterval(MISa_all.fixef)

##among-indiv variance
MISa_all.bID <- MISa_all.sim@ranef$squirrel_id
MISa_all.bvar <- as.vector(apply(MISa_all.bID, 1, var))
MISa_all.bvar <- coda::as.mcmc(MISa_all.bvar)
MCMCglmm::posterior.mode(MISa_all.bvar)
coda::HPDinterval(MISa_all.bvar)

##residual variance
MISa_all.rvar <- MISa_all.sim@sigma^2
MISa_all.rvar <- coda::as.mcmc(MISa_all.rvar)
MCMCglmm::posterior.mode(MISa_all.rvar)
coda::HPDinterval(MISa_all.rvar)

##repeatability
MISa_all.rID <- MISa_all.bvar/(MISa_all.bvar + MISa_all.rvar)
MCMCglmm::posterior.mode(MISa_all.rID)
coda::HPDinterval(MISa_all.rID)

