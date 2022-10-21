library(ggplot2)
library(paletteer)
library(gtools)
library(gridExtra)
library(ggpubr)

survival_residuals <- dat %>%
  filter(!is.na(growth_sc)) %>%
  mutate(survival_to_autumn_res = scale(predict(survival_to_autumn)),
         survival_to_200d_res = scale(predict(survival_to_200d)), 
         mastyear = factor(mastyear,
                           labels = c("non-mast year", "mast year")),
         binned_density = factor(cut_interval(grid_density, n = 2),
                                 labels = c("low", "high")),
         binned_mis1 = factor(cut_interval(mis1, n = 2),
                              labels = c("low", "high")))

survival_autumn_oftmis_fig <- ggplot(survival_residuals,
                                  aes(oft1, made_it, col = binned_mis1)) +
  theme_classic() +
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = binned_mis1),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  scale_color_paletteer_d("ggprism::viridis") + 
  labs(x = "Activity",
       y = "Probability of survival to autumn",
       col = "Aggression") 
plot(survival_autumn_oftmis_fig)

survival_autumn_mast_fig <- ggplot(survival_residuals,
                                  aes(mastyear, made_it, col = year)) +
  theme_classic() + 
  stat_boxplot(geom = "errorbar",
               width = 0.6) +
  geom_boxplot(aes(col = year)) +
  geom_jitter(aes(col = year)) +
  scale_color_paletteer_d("ggprism::viridis") + 
  labs(x = "Year type",
       y = "Probability of survival to autumn",
       col = "Year")
plot(survival_autumn_mast_fig)

survival_200d_oft_fig <- ggplot(survival_residuals,
                                  aes(oft1, survived_200d, col = binned_density)) +
  theme_classic() +
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = binned_density),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  scale_color_paletteer_d("ggprism::plasma") + 
  labs(x = "Activity",
       y = "Probability of survival to 200 days",
       col = "Grid density (Spring)")
plot(survival_200d_oft_fig)

survival_200d_mast_fig <- ggplot(survival_residuals,
                                  aes(mastyear, survived_200d, col = year)) +
  theme_classic() +
  stat_boxplot(geom = "errorbar",
               width = 0.6) +
  geom_boxplot(aes(col = year)) +
  geom_jitter(aes(col = year)) +
  scale_color_paletteer_d("ggprism::plasma") + 
  labs(x = "Year",
       y = "Probability of survival to 200 days",
       col = "Year")
plot(survival_200d_mast_fig)

ggsave("figures/survival~personality.png",
       arrangeGrob(survival_autumn_oft_fig, survival_autumn_mis_fig,
                   survival_200d_oft_fig, survival_200d_mis_fig,
                   ncol = 2,
                   nrow = 2,
                   top = "Effects of juvenile personality on survival"))
