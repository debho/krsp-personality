library(ggplot2)
library(paletteer)
library(gtools)
library(gridExtra)
library(ggpubr)
library(mgcv)

survival_residuals <- dat %>%
  filter(!is.na(growth_sc)) %>%
  mutate(survival_to_autumn_res = scale(predict(survival_to_autumn)),
         survival_to_200d_res = scale(predict(survival_to_200d)), 
         mastyear = factor(mastyear,
                           labels = c("non-mast year", "mast year")),
         binned_density = factor(cut_interval(grid_density, n = 2),
                                 labels = c("low", "high")),
         binned_oft1 = factor(cut_interval(oft1, n = 2),
                              labels = c("low", "high")),
         binned_mis1 = factor(cut_interval(mis1, n = 2),
                              labels = c("low", "high")))

# SURVIVAL ~ MAST YEAR ####
survival_autumn_mast_fig <- ggplot(survival_residuals,
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

survival_200d_mast_fig <- ggplot(survival_residuals,
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

# SURVIVAL ~ PERSONALITY ####
par(mfrow = c(1, 2))
autumn_personality <- gam(made_it ~ te(oft1, mis1), data = survival_residuals)
overwinter_personality <- gam(survived_200d ~ te(oft1, mis1), data = personality)
vis.gam(autumn_personality,
        plot.type = "persp",
        color = "heat",
        theta = 135,
        main = "(a.) Effect of personality traits on survival to autumn",
        xlab = "Activity",
        ylab = "Aggression",
        zlab = "Probability of survival to autumn")
vis.gam(overwinter_personality,
        plot.type = "persp",
        color = "heat",
        theta = 135,
        main = "(b.) Effect of personality traits on survival overwinter",
        xlab = "Activity",
        ylab = "Aggression",
        zlab = "Probability of surviving overwinter")
ggsave("figures/survival~personality.png")

# SURVIVAL ~ PERSONALITY x DENSITY ####
par(mfrow = c(2, 2))
autumn_activity <- gam(made_it ~
                         te(oft1, grid_density), data = personality)
autumn_aggression <- gam(made_it ~
                           te(mis1, grid_density), data = personality)
overwinter_activity <- gam(survived_200d ~
                             te(oft1, grid_density), data = personality)
overwinter_aggression <- gam(survived_200d ~
                               te(mis1, grid_density), data = personality)
vis.gam(autumn_activity,
        plot.type = "persp",
        color = "heat",
        theta = 135,
        main = "(a.) Effect of activity and density on survival to autumn",
        xlab = "Activity",
        ylab = "Density",
        zlab = "Probability of survival to autumn")
vis.gam(overwinter_activity,
        plot.type = "persp",
        color = "heat",
        theta = 135,
        main = "(c.) Effect of activity and density on survival overwinter",
        xlab = "Activity",
        ylab = "Density",
        zlab = "Probability of survival overwinter")
vis.gam(autumn_aggression,
        plot.type = "persp",
        color = "heat",
        theta = 135,
        main = "(b.) Effect of aggression and density on survival to autumn",
        xlab = "Aggression",
        ylab = "Density",
        zlab = "Probability of survival to autumn")
vis.gam(overwinter_aggression,
        plot.type = "persp",
        color = "heat",
        theta = 135,
        main = "(d.) Effect of aggression and density on survival overwinter",
        xlab = "Aggression",
        ylab = "Density",
        zlab = "Probability of survival overwinter")
ggsave("figures/survival~density.png")

survival_200d_oft_fig <- ggplot(survival_residuals,
                                  aes(oft1, survived_200d, col = binned_density)) +
  theme_classic() +
  labs_pubr() +
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = binned_density),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  scale_color_paletteer_d("ggprism::plasma") + 
  labs(x = "Activity",
       y = "Probability of overwinter survival",
       col = "Grid density (Spring)",
       tag = "c.")
plot(survival_200d_oft_fig)

ggsave("figures/survival~density.png",
       grid.arrange(autumn_activity,
                    autumn_aggression,
                    overwinter_activity,
                    overwinter_aggression,
                    ncol = 2,
                    nrow = 2))

       