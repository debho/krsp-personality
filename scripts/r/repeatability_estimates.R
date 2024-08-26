# runs repeatability estimates on trials 1 and 2 for juvs with at least 2 trials

#obtained personality2 by changing trialnumber == 2 in data-cleaning.R to extract all trial 2 juvs
personality2 <- personality2 %>%
  filter(squirrel_id %in% personality$squirrel_id) %>% #removes those missing trial 1
  drop_na(front) #removes those missing MIS

personality_repeat <- personality %>%
  filter(squirrel_id %in% personality2$squirrel_id) %>%
  bind_rows(personality2)

n_distinct(personality_repeat$squirrel_id)
#n = 60 juveniles


