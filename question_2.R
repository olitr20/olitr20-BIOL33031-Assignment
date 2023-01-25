library(tidyverse)
library(ggpubr)
library(afex)
library(emmeans)
library(patchwork)

### ANOVA ----------------------------------------------------------------------
assignment_dataset_2 <- read_csv("assignment_dataset_2.csv")

tidied_dataset_2 <- assignment_dataset_2 %>%
  mutate(condition = recode(condition,
    "condition_a" = "normal",
    "condition_b" = "degraded"
  )) %>%
  transmute(
    subject = factor(participant),
    condition = factor(condition,
      levels = c("normal", "degraded"),
    ),
    RT = response_time,
    caffeine = ordered(caffeine)
  )

contrasts(tidied_dataset_2$caffeine) <- contr.sum(7) # Use deviation coding to compare the given means of each level of caffeine to the overall mean for each condition. contr.sum is a sum contrast matrix

tidied_dataset_2 %>%
  group_by(condition, caffeine) %>%
  summarise(mean = mean(RT), sd = sd(RT))

condition_caffeine_plot <- tidied_dataset_2 %>%
  ggplot(aes(x = caffeine, y = RT)) +
  geom_violin(trim = FALSE, colour = "grey50", alpha = 0) +
  geom_jitter(width = .4, alpha = .65, aes(colour = condition)) +
  stat_summary(fun.data = "mean_cl_boot", colour = "grey25", linewidth = .75) +
  labs(
    x = "Cups of Coffee",
    y = "Response Time (ms)",
    col = "Condition:"
  ) +
  theme_pubr() +
  color_palette(
    palette = c("#FF2D79", "#009093"),
    labels = c("Normal", "Visually Degraded")
  ) +
  theme(text = element_text(size = 12))

condition_caffeine_plot

model_anova_2 <- aov_4(
  RT ~ condition + (1 | subject),
  data = tidied_dataset_2
)

model_ancova_2 <- aov_4(
  RT ~ condition + caffeine + (1 | subject),
  data = tidied_dataset_2,
  factorize = FALSE
)

anova(model_anova_2)
anova(model_ancova_2)

emmeans(model_ancova_2, pairwise ~ condition)

### Linear Model Equivalent ----------------------------------------------------
tidied_dataset_2 %>%
  group_by(condition, caffeine) %>%
  summarise(mean = mean(RT))

condition_plot <- tidied_dataset_2 %>%
  ggplot(aes(x = condition, y = RT, colour = condition)) +
  geom_violin(trim = FALSE, colour = "grey50", alpha = 0) +
  geom_jitter(width = .25, alpha = .65) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", linewidth = .75) +
  guides(colour = "none") +
  labs(x = "Word Presentation", y = "Response Time (ms)") +
  theme_pubr() +
  color_palette(palette = c("#FF2D79", "#009093")) +
  theme(text = element_text(size = 12))

condition_plot / condition_caffeine_plot

model_2_lm <- lm(
  RT ~ condition + caffeine,
  data = tidied_dataset_2
)

summary(model_2_lm)

### Other Contrast Models ------------------------------------------------------
forward.diff <- matrix(c(6/7, -1/7, -1/7, -1/7, -1/7, -1/7, -1/7,
                         5/7, 5/7, -2/7, -2/7, -2/7, -2/7, -2/7,
                         4/7, 4/7, 4/7, -3/7, -3/7, -3/7, -3/7,
                         3/7, 3/7, 3/7, 3/7, -4/7, -4/7, -4/7,
                         2/7, 2/7, 2/7, 2/7, 2/7, -5/7, -5/7,
                         1/7, 1/7, 1/7, 1/7, 1/7, 1/7, -6/7), ncol = 6)

backward.diff <- matrix(c(-6/7, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7,
                          -5/7, -5/7, 2/7, 2/7, 2/7, 2/7, 2/7,
                          -4/7, -4/7, -4/7, 3/7, 3/7, 3/7, 3/7,
                          -3/7, -3/7, -3/7, -3/7, 4/7, 4/7, 4/7,
                          -2/7, -2/7, -2/7, -2/7, -2/7, 5/7, 5/7,
                          -1/7, -1/7, -1/7, -1/7, -1/7, -1/7, 6/7), ncol = 6)
