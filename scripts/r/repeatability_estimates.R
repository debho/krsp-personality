# runs repeatability estimates on trials 1 and 2 for juvs with at least 2 trials

library(tidyverse)
library(lme4)

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

#non-adjusted repeatability
OFTna <- lmer(oft1 ~ (1|squirrel_id) + (1|gridyear),
               personality_repeat)
summary(OFTna)
plot(OFTna)
hist(resid(OFTna))

OFTna.sim <- arm::sim(OFTna, 1000)
OFTna.fixef = OFTna.sim@fixef
OFTna.ranef = OFTna.sim@ranef
OFTna.fixef = coda::as.mcmc(OFTna.fixef)
MCMCglmm::posterior.mode(OFTna.fixef)
coda::HPDinterval(OFTna.fixef)

##among-indiv variance
OFTna.bID <- OFTna.sim@ranef$squirrel_id
OFTna.bvar <- as.vector(apply(OFTna.bID, 1, var))
OFTna.bvar <- coda::as.mcmc(OFTna.bvar)
MCMCglmm::posterior.mode(OFTna.bvar)
coda::HPDinterval(OFTna.bvar)

##residual variance
OFTna.rvar <- OFTna.sim@sigma^2
OFTna.rvar <- coda::as.mcmc(OFTna.rvar)
MCMCglmm::posterior.mode(OFTna.rvar)
coda::HPDinterval(OFTna.rvar)

##repeatability
OFTna.rID <- OFTna.bvar/(OFTna.bvar + OFTna.rvar)
MCMCglmm::posterior.mode(OFTna.rID)
coda::HPDinterval(OFTna.rID)

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