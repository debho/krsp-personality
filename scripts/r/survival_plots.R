library(ggplot2)
library(paletteer)
library(gtools)
library(gridExtra)
library(ggpubr)

survival_residuals <- dat %>%
  filter(!is.na(growth_sc)) %>%
  mutate(survival_to_autumn_res = scale(predict(survival_to_autumn)),
         survival_to_200d_res = scale(predict(survival_to_200d)), 
         binned_density = factor(cut_interval(grid_density, n = 2),
                                 labels = c("low", "high")))

survival_autumn_oft_fig <- ggplot(survival_residuals,
                                  aes(oft1, made_it, col = binned_density)) +
  theme_classic() +
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = binned_density),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  scale_color_paletteer_d("colorBlindness::Blue2Gray8Steps") + 
  labs(x = "Activity",
       y = "Probability of survival to autumn",
       col = "Grid density (Spring)",
       tag = "a.")
plot(survival_autumn_oft_fig)

survival_autumn_mis_fig <- ggplot(survival_residuals,
                                  aes(mis1, made_it, col = binned_density)) +
  theme_classic() +
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = binned_density),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  scale_color_paletteer_d("colorBlindness::Blue2Gray8Steps") + 
  labs(x = "Aggression",
       y = "Probability of survival to autumn",
       col = "Grid density (Spring)",
       tag = "b.")
plot(survival_autumn_mis_fig)

survival_200d_oft_fig <- ggplot(survival_residuals,
                                  aes(oft1, survived_200d, col = binned_density)) +
  theme_classic() +
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = binned_density),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  scale_color_paletteer_d("colorBlindness::Blue2Orange8Steps") + 
  labs(x = "Activity",
       y = "Probability of survival to 200 days",
       col = "Grid density (Spring)",
       tag = "c.")
plot(survival_200d_oft_fig)

survival_200d_mis_fig <- ggplot(survival_residuals,
                                  aes(mis1, survived_200d, col = binned_density)) +
  theme_classic() +
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = binned_density),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  scale_color_paletteer_d("colorBlindness::Blue2Orange8Steps") + 
  labs(x = "Aggression",
       y = "Probability of survival to 200 days",
       col = "Grid density (Spring)",
       tag = "d.")
plot(survival_200d_mis_fig)

ggsave("figures/survival~personality.png",
       arrangeGrob(survival_autumn_oft_fig, survival_autumn_mis_fig,
                   survival_200d_oft_fig, survival_200d_mis_fig,
                   ncol = 2,
                   nrow = 2,
                   top = "Effects of juvenile personality on survival"))
