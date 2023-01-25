library(tidyverse)
library(ggpubr)
library(afex)
library(emmeans)


assignment_dataset_1 <- read_csv("assignment_dataset_1.csv")

tidied_dataset_1 <- assignment_dataset_1 %>%
  mutate(condition = recode(condition,
    "condition_a" = "normal",
    "condition_b" = "degraded"
  )) %>%
  transmute(
    subject = factor(participant),
    condition = factor(condition,
      levels = c("normal", "degraded"),
    ),
    RT = response_time
  )

tidied_dataset_1 %>%
  group_by(condition) %>%
  summarise(mean = mean(RT), sd = sd(RT))

tidied_dataset_1 %>%
  ggplot(aes(x = condition, y = RT, colour = condition)) +
  geom_violin(trim = FALSE, colour = "grey50", alpha = 0) +
  geom_jitter(width = .25, alpha = .65) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", linewidth = .75) +
  guides(colour = "none") +
  labs(x = "Word Presentation", y = "Response Time (ms)") +
  scale_x_discrete(labels = c("Normal", "Visually\nDegraded")) +
  theme_pubr() +
  color_palette(palette = c("#FF2D79", "#009093")) +
  theme(text = element_text(size = 12))

model_1 <- aov_4(
  RT ~ condition + (1 | subject),
  data = tidied_dataset_1
)

model_1_null <- aov_4(
  RT ~ 1 + (1 | subject),
  data = tidied_dataset_1
)

anova(model_1, model_1_null)

anova(model_1_null)

emmeans(model_1, pairwise ~ condition)

# The analyses were carried out using the afex package (Singmann H, Bolker B,
# Westfall J, Aust F & Ben-Shachar M, 2022) to fit the ANOVA model for the
# response time measure in R version 4.2.2 (R Development Core Team,
# 2020). Pairwise comparisons conducted with the emmeans package (Lenth R, 2022)
# were used to investigate the interaction for the response time measure.

# We found a significant effect of response time (F (1,94) = 15.828,
# p < .001, generalised η2 = .14411). Tukey comparisons revealed that the
# visually degraded group performed significantly worse than the normal group
# (18.1 ms slower, t = -3.978, p < 0.001).
