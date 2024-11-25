library(here)
source(here("scripts","data_preparation.R"))

data_clean <- data_clean %>%
  # standardization for probit to prob. 
  mutate(across(c(
    "age6", "q67_1", "q67_2", "q67_3", 
    "q67_4", "q67_5", "q41_1", "q41_2", "q41_5"
  ), ~ scale(., center = TRUE, scale = TRUE)))


mod_lm <- '
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
cfa_inteffa =~  "e1"*q41_1 + "e2"*q41_2 + "e3"*q41_5
  
  q78_1d ~ cfa_populism +
    lrcses_Right + 
    lrcses_Left + 
    edu3n_2 + 
    edu3n_3 + 
    q2 + 
    age6 + 
    q81 + 
    region + 
    cfa_inteffa
'

# Fit the SEM model
sem_lr_free <- cfa(mod_lm, data_clean, meanstructure = TRUE)

# Calculate R-squared for q78_1d
r2_f3 <- lavInspect(sem_lr_free, "rsquare")["q78_1d"] |> 
  unlist() |> 
  round(3)

# Sample size
sample_size <- sum(unlist(sem_lr_free@Data@nobs))

# Create the label for the plot
label <- paste0("N: ", sample_size, ", ", "$R^2$: ", r2_f3)

# Estimated marginal means for Populism
slope <- emmeans(sem_lr_free, "cfa_populism", 
                 lavaan.DV = "q78_1d", 
                 at = list(cfa_populism = seq(-2, 2, 0.01)),
                 rg.limit = 60000) |> 
  data.frame()


# Plot of predicted probabilities
plot_pp <- ggplot(slope, aes(x = cfa_populism, y = emmean)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), fill = "grey75") +
  geom_line(color = "black") +
  theme_classic() +
  labs(x = "Populist Attitudes", 
       y = "Pr(Y = 'Everyone should be allowed to say what he/she wants')") +
  patchwork::plot_annotation(
    caption = latex2exp::TeX(label),
    theme = theme(plot.title = element_text(hjust = 0.5), 
                  plot.subtitle = element_text(hjust = 0.5))
  )

# Save the plot
ggsave("figures/sem_q78_1d_pp.png", plot = plot_pp, 
       width = 25, height = 13, units = "cm")
