library(tidyverse)
library(ggpubr)
library(afex)
library(lme4)
library(lmerTest)
library(performance)
library(emmeans)


### Tidying & Summarisation ----------------------------------------------------
assignment_dataset_3 <- read_csv("assignment_dataset_3.csv")

cleaned_dataset_3 <- assignment_dataset_3 %>%
  pivot_longer(cols = c("positiveprime_positivetarget",
                        "positiveprime_negativetarget",
                        "negativeprime_positivetarget",
                        "negativeprime_negativetarget"),
               names_to = "condition",
               values_to = "response_time") %>%
  separate(condition, into = c("prime", "target"))

tidied_dataset_3 <- cleaned_dataset_3 %>%
  mutate(prime = recode(prime,
                        "positiveprime" = "positive",
                        "negativeprime" = "negative"),
         target = recode(target,
                         "positivetarget" = "positive",
                         "negativetarget" = "negative")) %>%
  transmute(subject = factor(participant),
            RT = response_time,
            prime = factor(prime),
            target = factor(target))

contrasts(tidied_dataset_3$prime) <- matrix(c(0.5, -0.5))
contrasts(tidied_dataset_3$target) <- matrix(c(0.5, -0.5))

tidied_dataset_3 %>%
  group_by(prime, target) %>%
  summarise(mean = mean(RT), sd = sd(RT))

tidied_dataset_3 %>%
  ggplot(aes(x = prime:target, y = RT, colour = prime:target)) +
  geom_violin(width = .8, trim = FALSE, colour = "grey50", alpha = 0) +
  geom_jitter(width = .2, alpha = .5) +
  stat_summary(fun.data = "mean_cl_boot", colour = "grey25", linewidth = .75) +
  guides(colour = "none") +
  labs(x = "Condition",
       y = "Response Time (ms)") +
  scale_x_discrete(labels = c("Negative\nX\nNegative",
                              "Negative\nX\nPositive",
                              "Positive\nX\nNegative",
                              "Positive\nX\nPositive")) +
  theme_pubr() +
  color_palette(palette = c("#FF2D79", "#009093", "#531B92", "#76D6FE")) +
  theme(text = element_text(size = 12))


### General Linear Model -------------------------------------------------------
model_3_glm <- aov_4(RT ~ prime * target +
                     (1 + prime * target | subject),
                   data = tidied_dataset_3)

anova(model_3_glm)

emmeans(model_3_glm, pairwise ~ prime * target, adjust = "none")


### Linear Mixed Model ---------------------------------------------------------

model_3_lmm <- lmer(
  RT ~ prime * target +
    (1 + prime * target | subject),
  data = tidied_dataset_3
) # Insufficient observations to support the model

model_3_lmm <- lmer(
  RT ~ prime * target +
    (1 + prime + target | subject),
  data = tidied_dataset_3
) # Doesn't converge

summary(model_3_lmm) # Least variance comes from the 'prime' factor

model_3_lmm <- lmer(
  RT ~ prime * target +
    (1 + target | subject),
  data = tidied_dataset_3
) # Now it converges

check_model(model_3_lmm)

anova(model_3_lmm)

emmeans(model_3_lmm, pairwise ~ prime * target, adjust = "none")


### Buildmer Package Testing ---------------------------------------------------
# https://cran.r-project.org/web/packages/buildmer/vignettes/buildmer.html
# https://mspeekenbrink.github.io/sdam-r-companion/linear-mixed-effects-models.html#automatically-finding-optimal-random-effects-structures-with-the-buildmer-package

library(lme4)
library(buildmer)

model_buildmer <- buildmer(
  RT ~ prime * target +
    (1 + prime * target | subject),
  data = tidied_dataset_3,
  buildmerControl = buildmerControl(direction = c("backward"))
)

formula(model_buildmer@model)

model_buildmer_lm <- lm(
  RT ~ prime * target,
  data = tidied_dataset_3
)

anova(model_3_lmm, model_buildmer_lm) # not sure if this is great because it doesn't work as anova(model_buildmer_lm, model_3_lmm)

anova(model_buildmer_lm)

emmeans(model_buildmer_lm, pairwise ~ prime * target, adjust = "none")
