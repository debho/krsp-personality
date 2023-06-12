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
  scale_color_paletteer_d("ggprism::colorblind_safe") + 
  geom_violin(alpha = 0.8) +
  geom_boxplot(alpha = 0.3) +
  scale_fill_paletteer_d("ggprism::magma") +
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
  scale_color_paletteer_d("ggprism::colorblind_safe") + 
  geom_violin(alpha = 0.8) +
  geom_boxplot(alpha = 0.3) +
  scale_fill_paletteer_d("ggprism::magma") +
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

# FIGURE 1: activity x aggression ####
autumn_personality_scatter_fig <- visreg(survival_to_autumn,
                                         "mis1", by = "oft1",
                                         gg = T, overlay = T,
                                         xlab = "Aggression",
                                         ylab = "Probability of survival to autumn",
                                         point = list(alpha = 0.8),
                                         fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::colorblind_safe",
                          labels = c("Low Activity",
                                     "Medium Activity",
                                     "High Activity")) +
  scale_fill_paletteer_d("ggprism::colorblind_safe",
                         labels = c("Low Activity",
                                    "Medium Activity",
                                    "High Activity")) + 
  labs(color = "Activity",
       fill = "Activity") +
  theme_classic() +
  labs_pubr()

overwinter_personality_scatter_fig <- visreg(survival_to_200d,
                                             "mis1", by = "oft1",
                                             gg = T, overlay = T,
                                             xlab = "Aggression",
                                             ylab = "Probability of survival overwinter",
                                             point = list(alpha = 0.8),
                                             fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::viridis",
                          labels = c("Low Activity",
                                     "Medium Activity",
                                     "High Activity")) +
  scale_fill_paletteer_d("ggprism::viridis",
                         labels = c("Low Activity",
                                    "Medium Activity",
                                    "High Activity")) +
  labs(color = "Activity",
       fill = "Activity") +
  theme_classic() +
  labs_pubr()

ggsave("figures/survival_personality.png",
       grid.arrange(autumn_personality_scatter_fig,
                    overwinter_personality_scatter_fig,
                    ncol = 1,
                    nrow = 2))

autumn_personality <- gam(made_it ~ te(oft1, mis1), data = dat)
overwinter_personality <- gam(survived_200d ~ te(oft1, mis1), data = dat2)
par(mfcol = c(2,2))
vis.gam(autumn_personality,
        plot.type = "persp",
        color = "topo",
        theta = 135,
        ticktype = "detailed",
        xlab = "Activity",
        ylab = "Aggression",
        zlab = "Probability of survival to autumn")
vis.gam(overwinter_personality,
        plot.type = "persp",
        color = "topo",
        theta = 135,
        ticktype = "detailed",
        xlab = "Activity",
        ylab = "Aggression",
        zlab = "Probability of surviving overwinter")

summary(autumn_personality)
summary(overwinter_personality)

# FIGURE 2: personality x density ####
autumn_activity <- gam(made_it ~
                         te(oft1, grid_density), data = dat)
autumn_aggression <- gam(made_it ~
                           te(mis1, grid_density), data = dat)
overwinter_activity <- gam(survived_200d ~
                             te(oft1, grid_density), data = dat)
overwinter_aggression <- gam(survived_200d ~
                               te(mis1, grid_density), data = dat)
par(mfcol = c(2,2))
vis.gam(autumn_activity,
        plot.type = "persp",
        color = "topo",
        theta = -35,
        ticktype = "detailed",
        xlab = "Activity",
        ylab = "Density",
        zlab = "Probability of survival to autumn") 
vis.gam(autumn_aggression,
        plot.type = "persp",
        color = "topo",
        theta = -35,
        ticktype = "detailed",
        xlab = "Aggression",
        ylab = "Density",
        zlab = "Probability of survival to autumn")
par(mfcol = c(2,2))
vis.gam(overwinter_activity,
        plot.type = "persp",
        color = "topo",
        theta = -35,
        ticktype = "detailed",
        xlab = "Activity",
        ylab = "Density",
        zlab = "Probability of survival overwinter")
vis.gam(overwinter_aggression,
        plot.type = "persp",
        color = "topo",
        theta = -35,
        ticktype = "detailed",
        xlab = "Aggression",
        ylab = "Density",
        zlab = "Probability of survival overwinter")

summary(autumn_activity)
summary(overwinter_activity)
summary(autumn_aggression)
summary(overwinter_aggression)

autumn_activity_scatter_fig <- visreg(survival_to_autumn,
                                      "grid_density", by = "oft1",
                                      gg = T, overlay = T,
                                      xlab = "Density",
                                      ylab = "Probability of survival to autumn",
                                      point = list(alpha = 0.5),
                                      fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::colorblind_safe",
                          labels = c("Low Activity",
                                     "Medium Activity",
                                     "High Activity")) +
  scale_fill_paletteer_d("ggprism::colorblind_safe",
                         labels = c("Low Activity",
                                    "Medium Activity",
                                    "High Activity")) +
  labs(color = "Activity",
       fill = "Activity") +
  theme_classic() +
  labs_pubr()

autumn_aggression_scatter_fig <- visreg(survival_to_autumn,
                                        "grid_density", by = "mis1",
                                        gg = T, overlay = T,
                                        xlab = "Density",
                                        ylab = "Probability of survival to autumn",
                                        point = list(alpha = 0.5),
                                        fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::colorblind_safe",
                          labels = c("Low Aggression",
                                     "Medium Aggression",
                                     "High Aggression")) +
  scale_fill_paletteer_d("ggprism::colorblind_safe",
                         labels = c("Low Aggression",
                                    "Medium Aggression",
                                    "High Aggression")) +
  labs(color = "Aggression",
       fill = "Aggression") +
  theme_classic() +
  labs_pubr()

overwinter_activity_scatter_fig <- visreg(survival_to_200d,
       "grid_density", by = "oft1",
       gg = T, overlay = T,
       xlab = "Density",
       ylab = "Probability of survival overwinter",
       point = list(alpha = 0.5),
       fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::viridis",
                          labels = c("Low Activity",
                                     "Medium Activity",
                                     "High Activity")) +
  scale_fill_paletteer_d("ggprism::viridis",
                         labels = c("Low Activity",
                                    "Medium Activity",
                                    "High Activity")) +
  labs(color = "Activity",
       fill = "Activity") +
  theme_classic() +
  labs_pubr()

overwinter_aggression_scatter_fig <- visreg(survival_to_200d,
       "grid_density",by = "mis1",
       gg = T, overlay = T,
       xlab = "Density",
       ylab = "Probability of survival overwinter",
       point = list(alpha = 0.5),
       fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::viridis",
                          labels = c("Low Aggression",
                                     "Medium Aggression",
                                     "High Aggression")) +
  scale_fill_paletteer_d("ggprism::viridis",
                         labels = c("Low Aggression",
                                    "Medium Aggression",
                                    "High Aggression")) +
  labs(color = "Aggression",
       fill = "Aggression") +
  theme_classic() +
  labs_pubr()

ggsave("figures/survival~autumndensity.png",
       grid.arrange(autumn_activity_scatter_fig,
                    autumn_aggression_scatter_fig,
                    ncol = 1,
                    nrow = 2))

ggsave("figures/survival~overwinterdensity.png",
       grid.arrange(overwinter_activity_scatter_fig,
                    overwinter_aggression_scatter_fig,
                    ncol = 1,
                    nrow = 2))

# FIGURE ???: growth rate x density ####  

growth_autumn_fig <- visreg(survival_to_autumn,
                            "grid_density", by = "growth_sc",
                            gg = T, overlay = T,
                            xlab = "Density",
                            ylab = "Probability of survival to autumn",
                            point = list(alpha = 0.5),
                            fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::colorblind_safe",
                          labels = c("Low Growth Rate",
                                     "Medium Growth Rate",
                                     "High Growth Rate")) +
  scale_fill_paletteer_d("ggprism::colorblind_safe",
                         labels = c("Low Growth Rate",
                                    "Medium Growth Rate",
                                    "High Growth Rate")) +
  labs(color = "Growth Rate",
       fill = "Growth Rate") +
  theme_classic() +
  labs_pubr()

growth_overwinter_fig <- visreg(survival_to_200d,
                                            "grid_density",by = "growth_sc",
                                            gg = T, overlay = T,
                                            xlab = "Density",
                                            ylab = "Probability of survival overwinter",
                                            point = list(alpha = 0.5),
                                            fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::viridis",
                          labels = c("Low Growth Rate",
                                     "Medium Growth Rate",
                                     "High Growth Rate")) +
  scale_fill_paletteer_d("ggprism::viridis",
                         labels = c("Low Growth Rate",
                                    "Medium Growth Rate",
                                    "High Growth Rate")) +
  labs(color = "Growth Rate",
       fill = "Growth Rate") +
  theme_classic() +
  labs_pubr()

ggsave("figures/survival~growthSCATTER.png",
       grid.arrange(growth_autumn_fig,
                    growth_overwinter_fig,
                    ncol = 2,
                    nrow = 1))
