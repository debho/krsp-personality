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
         binned_oft1 = factor(cut_interval(oft1, n = 2),
                              labels = c("low", "high")),
         binned_mis1 = factor(cut_interval(mis1, n = 2),
                              labels = c("low", "high")))

survival_autumn_oftmis_fig <- ggplot(survival_residuals,
                                  aes(oft1, made_it, col = binned_mis1)) +
  theme_classic() +
  labs_pubr() +
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = binned_mis1),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  scale_color_paletteer_d("ggprism::viridis") + 
  labs(x = "Activity",
       y = "Probability of survival to autumn",
       col = "Aggression",
       tag = "a.") 
plot(survival_autumn_oftmis_fig)

survival_autumn_mast_fig <- ggplot(survival_residuals,
                                 aes(mastyear, made_it, fill = mastyear)) +
  theme_classic() +
  labs_pubr() +
  geom_jitter(aes(col = year)) +
  scale_color_paletteer_d("ggprism::viridis") + 
  geom_violin(alpha = 0.8) +
  scale_fill_paletteer_d("ggprism::pastels") +
  labs(x = "Mast Year",
       y = "Probability of survival to autumn",
       col = "Year",
       fill = "Mast Year",
       tag = "a.")
plot(survival_autumn_mast_fig)

survival_200d_oftmis_fig <- ggplot(survival_residuals,
                                aes(oft1, survived_200d, col = binned_mis1)) +
  theme_classic() +
  labs_pubr() +
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = binned_mis1),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  scale_color_paletteer_d("ggprism::viridis") + 
  labs(x = "Activity",
       y = "Probability of survival to 200 days",
       col = "Aggression",
       tag = "b.")
plot(survival_200d_oftmis_fig)

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
       y = "Probability of survival to 200 days",
       col = "Grid density (Spring)",
       tag = "c.")
plot(survival_200d_oft_fig)

survival_200d_mast_fig <- ggplot(survival_residuals,
                                  aes(mastyear, survived_200d, fill = mastyear)) +
  theme_classic() +
  labs_pubr() +
  geom_jitter(aes(col = year)) +
  scale_color_paletteer_d("ggprism::viridis") + 
  geom_violin(alpha = 0.8) +
  scale_fill_paletteer_d("ggprism::pastels") +
  labs(x = "Mast Year",
       y = "Probability of survival to 200 days",
       col = "Year",
       fill = "Mast Year",
       tag = "b.")
plot(survival_200d_mast_fig)

ggsave("figures/survival~oftmis.png",
       grid.arrange(arrangeGrob(survival_autumn_oftmis_fig,
                                survival_200d_oftmis_fig),
                    survival_200d_oft_fig,
                    ncol = 2,
                    nrow = 1))
ggsave("figures/survival~mast.png",
       grid.arrange(survival_autumn_mast_fig,
                    survival_200d_mast_fig,
                    ncol = 2,
                    nrow = 1))
       