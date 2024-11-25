library(here)
source(here("scripts","data_preparation.R"))
# estimate factor scores
source(here::here("scripts", "APPENDIX_factor_scores_for_glm.R"))

data_clean_glmlr <- data_clean %>%
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

lm_sum <- glm(q78_1d ~ fs_populism + 
                q81 +
                region +
                q2 +  
                age6 +  
                edu3n_2 +
                edu3n_3 +
                lrcses +
                fs_inteff +
                fs_populism*lrcses +
                fs_populism*lrcses,
              data = data_clean_glmlr, 
              family = binomial(link = "probit"))

ggme_bind <- ggeffects::ggeffect(
  lm_sum
  , terms = c("fs_populism[-2.5:2.5 by=.1]","lrcses" )
) |> 
  as.data.frame()

# Calculate sample size and R-squared for annotation
sample_size <- broom::glance(lm_sum)$nobs
r_2 <- round(performance::r2(lm_sum)$R2, 3)
label <- paste0("N: ", sample_size, ", $R^2:$ ", r_2)

# Create black-and-white plot with no color fills
plot_affective <- ggplot(ggme_bind, aes(x = x, y = predicted, linetype = group)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, linetype = group),
              alpha = 0.1, fill = "grey") +
  geom_line(color = "black") +
  theme_classic() +
  geom_hline(yintercept = 0.5, color = 'grey40', linetype = "dashed", size = 0.5) +
  ylab("Probability of agreeing with 'Everyone can say what they want, even if this hurts others'") +
  xlab("Populist attitudes (factor scores)") +
  guides(linetype = guide_legend(title = "Ideological group")) +
  theme(
    axis.title = element_text(face = "bold"),
    strip.text.x = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  labs(linetype = NULL)

# Add caption and customize title positioning with patchwork
plot_facet <- plot_affective + patchwork::plot_annotation(
  caption = latex2exp::TeX(label)
)

# Save the final plot to file
ggsave(
  filename = "figures/appendix_glm_q78_1dXLR.png",
  plot = plot_facet,
  width = 30,
  height = 20,
  units = "cm"
)