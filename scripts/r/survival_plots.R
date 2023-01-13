library(ggplot2)
library(paletteer)
library(gtools)
library(gridExtra)
library(ggpubr)
library(mgcv)
library(visreg)

# mastyear figs ####
survival_autumn_mast_fig <- ggplot(dat,
                                   aes(mastyear, made_it, fill = mastyear)) +
  theme_classic() +
  labs_pubr() +
  geom_jitter(aes(col = year)) +
  scale_color_paletteer_d("ggprism::viridis") + 
  geom_violin(alpha = 0.8) +
  geom_boxplot(alpha = 0.3) +
  scale_fill_paletteer_d("ggprism::pastels") +
  labs(x = "Mast Year",
       y = "Probability of survival to autumn",
       col = "Year",
       fill = "Mast Year",
       tag = "a.")
plot(survival_autumn_mast_fig)

survival_200d_mast_fig <- ggplot(dat,
                                 aes(mastyear, survived_200d, fill = mastyear)) +
  theme_classic() +
  labs_pubr() +
  geom_jitter(aes(col = year)) +
  scale_color_paletteer_d("ggprism::viridis") + 
  geom_violin(alpha = 0.8) +
  geom_boxplot(alpha = 0.3) +
  scale_fill_paletteer_d("ggprism::pastels") +
  labs(x = "Mast Year",
       y = "Probability of overwinter survival",
       col = "Year",
       fill = "Mast Year",
       tag = "b.")
plot(survival_200d_mast_fig)

ggsave("figures/survival~mast.png",
       grid.arrange(survival_autumn_mast_fig,
                    survival_200d_mast_fig,
                    ncol = 2,
                    nrow = 1))

# 3D FIGS ####
# oft x mis 3D ####
par(mfrow = c(1, 2))
autumn_personality <- gam(made_it ~ te(oft1, mis1), data = dat)
overwinter_personality <- gam(survived_200d ~ te(oft1, mis1), data = dat)
vis.gam(autumn_personality,
        plot.type = "persp",
        color = "heat",
        theta = 135,
        main = "(a.)",
        ticktype = "detailed",
        xlab = "Activity",
        ylab = "Aggression",
        zlab = "Probability of survival to autumn")
vis.gam(overwinter_personality,
        plot.type = "persp",
        color = "heat",
        theta = 135,
        main = "(b.)",
        ticktype = "detailed",
        xlab = "Activity",
        ylab = "Aggression",
        zlab = "Probability of surviving overwinter")

summary(autumn_personality)
summary(overwinter_personality)

# personality x density 3D ####
par(mfrow = c(2, 2))
autumn_activity <- gam(made_it ~
                         te(oft1, grid_density), data = dat)
autumn_aggression <- gam(made_it ~
                           te(mis1, grid_density), data = dat)
overwinter_activity <- gam(survived_200d ~
                             te(oft1, grid_density), data = dat)
overwinter_aggression <- gam(survived_200d ~
                               te(mis1, grid_density), data = dat)
vis.gam(autumn_activity,
        plot.type = "persp",
        color = "heat",
        theta = 135,
        main = "(a.)",
        ticktype = "detailed",
        xlab = "Activity",
        ylab = "Density",
        zlab = "Probability of survival to autumn") 
vis.gam(overwinter_activity,
        plot.type = "persp",
        color = "heat",
        theta = 135,
        main = "(c.)",
        ticktype = "detailed",
        xlab = "Activity",
        ylab = "Density",
        zlab = "Probability of survival overwinter")
vis.gam(autumn_aggression,
        plot.type = "persp",
        color = "heat",
        theta = 135,
        main = "(b.)",
        ticktype = "detailed",
        xlab = "Aggression",
        ylab = "Density",
        zlab = "Probability of survival to autumn")
vis.gam(overwinter_aggression,
        plot.type = "persp",
        color = "heat",
        theta = 135,
        main = "(d.)",
        ticktype = "detailed",
        xlab = "Aggression",
        ylab = "Density",
        zlab = "Probability of survival overwinter")

summary(autumn_activity)
summary(overwinter_activity)
summary(autumn_aggression)
summary(overwinter_aggression)

# SCATTERPLOTS ####
dat$Activity <- cut(dat$oft1, 3,
                      labels = c("Low Activity", "Medium Activity", "High Activity"))
dat$Density <- cut(dat$grid_density, 3,
                   c("Low Density", "Medium Density", "High Density"))

# oft x mis models ####
autumn_personality_scatter <- glmer(made_it ~
                                   mis1*Activity +
                                   (1|year) +
                                   (1|dam_id) +
                                   (1|litter_id),
                                 data = dat,
                                 na.action = 'na.omit',
                                 family = binomial(link = "logit"),
                                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))
overwinter_personality_scatter <- glmer(survived_200d ~
                                  mis1*Activity +
                                  (1|year) +
                                  (1|dam_id) +
                                  (1|litter_id),
                                data = dat,
                                na.action = 'na.omit',
                                family = 'binomial',
                                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))

# oft x mis figs #### 
autumn_personality_scatter_fig <- visreg(autumn_personality_scatter,
                                         "mis1", by = "Activity",
                                         gg = T, overlay = T,
                                         xlab = "Aggression",
                                         ylab = "Probability of survival to autumn") +
  theme_bw() +
  labs_pubr()
overwinter_personality_scatter_fig <- visreg(overwinter_personality_scatter,
                                             "mis1", by = "Activity",
                                             gg = T, overlay = T,
                                             xlab = "Aggression",
                                             ylab = "Probability of survival to autumn") +
  theme_bw() +
  labs_pubr()

car::Anova(autumn_personality_scatter)
car::Anova(overwinter_personality_scatter)
summary(autumn_personality_scatter)
summary(overwinter_personality_scatter)

ggsave("figures/survival~personalitySCATTER.png",
       grid.arrange(autumn_personality_scatter_fig,
                    overwinter_personality_scatter_fig,
                    ncol = 2,
                    nrow = 1))

# personality x density models ####
autumn_activity_scatter <- glmer(made_it ~
                                   oft1*Density + 
                                   (1|year) +
                                   (1|dam_id) + 
                                   (1|litter_id), 
                                 data = dat,
                                 na.action = 'na.omit',
                                 family = binomial(link = "logit"),
                                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))

autumn_aggression_scatter <- glmer(made_it ~
                                     mis1*Density + 
                                     (1|year) +
                                     (1|dam_id) + 
                                     (1|litter_id), 
                                   data = dat,
                                   na.action = 'na.omit',
                                   family = binomial(link = "logit"),
                                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))

overwinter_activity_scatter <- glmer(survived_200d ~ 
                                       oft1*Density + 
                                       (1|year) +
                                       (1|dam_id) + 
                                       (1|litter_id),
                                     data = dat,
                                     na.action = 'na.omit',
                                     family = 'binomial',
                                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))

overwinter_aggression_scatter <- glmer(survived_200d ~ 
                                         mis1*Density + 
                                         (1|year) +
                                         (1|dam_id) + 
                                         (1|litter_id),
                                       data = dat,
                                       na.action = 'na.omit',
                                       family = 'binomial',
                                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))

# personality x density figs ####

autumn_activity_scatter_fig <- visreg(autumn_activity_scatter,
                                      "oft1", by = "Density",
                                      gg = T, overlay = T,
                                      xlab = "Activity",
                                      ylab = "Probability of survival to autumn",
                                      point = list(alpha = 0.5),
                                      fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::inferno") +
  scale_fill_paletteer_d("ggprism::inferno") +
  theme_bw() +
  labs_pubr()

autumn_aggression_scatter_fig <- visreg(autumn_aggression_scatter,
                                        "mis1", by = "Density",
                                        gg = T, overlay = T,
                                        xlab = "Aggression",
                                        ylab = "Probability of survival to autumn",
                                        point = list(alpha = 0.5),
                                        fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::inferno") +
  scale_fill_paletteer_d("ggprism::inferno") +
  theme_bw() +
  labs_pubr()

overwinter_activity_scatter_fig <- visreg(overwinter_activity_scatter,
       "oft1", by = "Density",
       gg = T, overlay = T,
       xlab = "Activity",
       ylab = "Probability of survival overwinter",
       point = list(alpha = 0.5),
       fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::floral") +
  scale_fill_paletteer_d("ggprism::floral") +
  theme_bw() +
  labs_pubr()

overwinter_aggression_scatter_fig <- visreg(overwinter_aggression_scatter,
       "mis1",by = "Density",
       gg = T, overlay = T,
       xlab = "Aggression",
       ylab = "Probability of survival overwinter",
       point = list(alpha = 0.5),
       fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::floral") +
  scale_fill_paletteer_d("ggprism::floral") +
  theme_bw() +
  labs_pubr()

summary(autumn_activity_scatter)
summary(overwinter_activity_scatter)
summary(autumn_aggression_scatter)
summary(overwinter_aggression_scatter)

ggsave("figures/survival~densitySCATTER.png",
       grid.arrange(autumn_activity_scatter_fig,
                    autumn_aggression_scatter_fig,
                    overwinter_activity_scatter_fig,
                    overwinter_aggression_scatter_fig,
                    ncol = 2,
                    nrow = 2))
