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
OFTrep_a <- lmer(oft1 ~ trialnumber + sex +
                   (1|squirrel_id) + (1|gridyear),
                  personality_repeat)
summary(OFTrep_a)
plot(OFTrep_a)
hist(resid(OFTrep_a))




###############
#### MIS 1 ####
###############

#non-adjusted repeatability

MISrep_na <- lmer(mis1 ~ (1|squirrel_id) + (1|gridyear),
                  personality_repeat)
summary(MISrep_na)
plot(MISrep_na)
hist(resid(MISrep_na))

#adjusted repeatability

MISrep_a <- lmer(mis1 ~ trialnumber + sex +
                   (1|squirrel_id) + (1|gridyear),
                 personality_repeat)
summary(MISrep_a)
plot(MISrep_a)
hist(resid(MISrep_a))
