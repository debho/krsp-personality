# runs repeatability estimates on trials 1 and 2 for juvs with at least 2 trials

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

#non-adjusted repeatability
OFTrep_na <- lmer(oft1 ~ (1|squirrel_id) + (1|gridyear),
               personality_repeat)
summary(OFTrep_na)
plot(OFTrep_na)
hist(resid(OFTrep_na))

MISrep_na <- lmer(mis1 ~ (1|squirrel_id) + (1|gridyear),
                  personality_repeat)
summary(MISrep_na)
plot(MISrep_na)
hist(resid(MISrep_na))

#adjusted repeatability
OFTrep_a <- lmer(oft1 ~ trialnumber + sex +
                   (1|squirrel_id) + (1|gridyear),
                  personality_repeat)
summary(OFTrep_a)
plot(OFTrep_a)
hist(resid(OFTrep_a))
confint(OFTrep_a,
        method = "Wald")

MISrep_a <- lmer(mis1 ~ trialnumber + sex +
                   (1|squirrel_id) + (1|gridyear),
                 personality_repeat)
summary(MISrep_a)
plot(MISrep_a)
hist(resid(MISrep_a))
confint(MISrep_a,
        method = "Wald")

