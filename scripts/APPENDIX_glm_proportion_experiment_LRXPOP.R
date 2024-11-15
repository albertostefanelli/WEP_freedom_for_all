library(here)
source(here("scripts","data_preparation.R"))
# estimate factor scores
source(here::here("scripts", "APPENDIX_factor_scores_for_glm.R"))

data_clean_glm <- data_clean %>%
  drop_na(q81, 
          region, 
          q2, 
          age6, 
          edu3n_2, 
          edu3n_3, 
          lrcses_Left, 
          lrcses_Right, 
          q41_5, 
          q41_2, 
          q41_1,
          q67_1,
          q67_2,
          q67_3,
          q67_4,
          q67_5)


# Run a logistic regression model without weights
lm_sum <- glm(
  expq80value ~ glrcond * fs_populism +
    glrcond * q81 +
    glrcond * region +
    glrcond * q2 +
    glrcond * age6 +
    glrcond * edu3n_2 +
    glrcond * edu3n_3 +
    glrcond * fs_inteff,
  data = data_clean_glm,
  family = binomial(link = "probit")
)

# Calculate estimated marginal means and transform results into a tidy format
slope_test_pvalue <- emmeans::emmeans(
  lm_sum,
  "fs_populism",
  by = "glrcond",
  at = list(fs_populism = c(-1, 0, 1)),
  type = "response",
  adjust = "none"
) %>%
  emmeans::test() %>%
  as_tibble()

# Extract and clean factor levels
slope_test_pvalue <- slope_test_pvalue %>%
  mutate(
    lr = factor(gsub("(.+?)(\\_.*)", "\\1", glrcond), 
                levels = c("Left", "Center", "Right"), ordered = TRUE),
    cond = gsub(".*_", "\\1", glrcond),
    popfs = case_when(
      fs_populism == -1 ~ "Low (-1 Sd)",
      fs_populism == 0 ~ "Average",
      fs_populism == 1 ~ "High (+1 Sd)"
    ) %>% factor(levels = c("Low (-1 Sd)", "Average", "High (+1 Sd)"), ordered = TRUE)
  )

# Plot the results with ggplot2
slope_test_pvalue %>%
  ggplot(aes(x = popfs, y = prob)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  geom_text(aes(label = paste0(round(prob * 100, 2), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, 
            color = 'black') + 
  labs(
    x = "Populist Attitudes",
    y = "Marginal probability of allowing a speech against [immigrants/multinationals]"
  ) +
  facet_grid(lr ~ cond) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent)

# Save the plot
ggsave(
  filename = "figures/appendix_glm_proportions_LRXPOP.png",
  width = 18,
  height = 15,
  units = "cm"
)

