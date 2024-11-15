library(here)
source(here("scripts","data_preparation.R"))

# Fit probit model
lm_sum <- glm(
  expq80value ~ glrcond,
  data = data_clean, 
  family = binomial(link = "probit")
)

# Calculate estimated marginal means and p-values
slope_test_pvalue <- emmeans(lm_sum, "glrcond", 
                             type = "response",
                             adjust = "none") %>% 
  test() %>% 
  as_tibble() %>% 
  # Extract ideological group and condition
  mutate(
    lr = factor(gsub("(.+?)(\\_.*)", "\\1", glrcond), 
                levels = c("Left", "Center", "Right"), ordered = TRUE),
    cond = gsub(".*_", "", glrcond)
  ) %>% 
  as.data.frame()

# Plot estimated probabilities by ideological group and condition
slope_test_pvalue %>%
  ggplot(aes(y = prob, x = lr, fill = lr)) + 
  geom_bar(position = "dodge", stat = "identity", color = "black") + 
  geom_text(aes(label = paste0(round(prob * 100, 2), "%")),
            position = position_stack(vjust = 0.5), color = 'black', size = 3) + 
  labs(
    x = "Ideological groups",
    y = "Probability of allowing speech against [immigrants/multinationals]"
  ) +
  facet_wrap(~cond) +
  scale_fill_grey(start = 0.3, end = 0.7, guide = "none") +  # Set black and white colors and remove legend
  theme_classic() +
  scale_y_continuous(labels = scales::percent)

# Save plot
ggsave("figures/appendix_glm_proportions_LR.png", 
       width = 15, height = 13, units = "cm")
