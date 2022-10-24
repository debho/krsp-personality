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
                                  aes(mis1, made_it, col = binned_oft1)) +
  theme_classic() +
  labs_pubr() +
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = binned_mis1),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  scale_color_paletteer_d("ggprism::viridis") + 
  labs(title = "Effect of personality traits on survival to autumn",
       x = "Aggression",
       y = "Probability of survival to autumn",
       col = "Activity",
       tag = "a.") 
plot(survival_autumn_oftmis_fig)

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
  labs(title = "Effect of activity on survival to 200 days",
       x = "Activity",
       y = "Probability of survival to 200 days",
       col = "Grid density (Spring)",
       tag = "b.")
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
       fill = "Mast Year")
plot(survival_200d_mast_fig)

ggsave("figures/survival~oftmis.png",
       grid.arrange(survival_autumn_oftmis_fig,
                    survival_200d_oft_fig,
                    ncol = 2,
                    nrow = 1))
ggsave("figures/survived200~mast.png",
       survival_200d_mast_fig)
       