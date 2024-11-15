library(here)
source(here("scripts","data_preparation.R"))

data_clean_multigroup <- data_clean %>%
  group_by(glrcond) %>%
  mutate(across(c(
    "age6", "id_str", "q67_1", "q67_2", "q67_3", 
    "q67_4", "q67_5", "q41_1", "q41_2", "q41_5"
  ), ~ scale(., center = TRUE, scale = TRUE))) %>%
  ungroup() 

mod_pop <- '

group: Right_Multinationals
cfa_populism =~ p1*q67_1 + p2*q67_2 + p3*q67_3 + p4*q67_4 + p5*q67_5
cfa_inteff =~ e1*q41_1 + e2*q41_2 + e3*q41_5
expq80value|"t1"*t1
expq80value ~ cfa_populism + q81 + region + q2 + age6 + edu3n_2 + edu3n_3 + cfa_inteff + id_str

group: Center_Multinationals
cfa_populism =~ p1*q67_1 + p2*q67_2 + p3*q67_3 + p4*q67_4 + p5*q67_5
cfa_inteff =~ e1*q41_1 + e2*q41_2 + e3*q41_5
expq80value|"t2"*t1
expq80value ~ cfa_populism + q81 + region + q2 + age6 + edu3n_2 + edu3n_3 + cfa_inteff

group: Left_Multinationals
cfa_populism =~ p1*q67_1 + p2*q67_2 + p3*q67_3 + p4*q67_4 + p5*q67_5
cfa_inteff =~ e1*q41_1 + e2*q41_2 + e3*q41_5
expq80value|"t3"*t1
expq80value ~ cfa_populism + q81 + region + q2 + age6 + edu3n_2 + edu3n_3 + cfa_inteff + id_str

group: Center_Immigrants
cfa_populism =~ p1*q67_1 + p2*q67_2 + p3*q67_3 + p4*q67_4 + p5*q67_5
cfa_inteff =~ e1*q41_1 + e2*q41_2 + e3*q41_5
expq80value|"t4"*t1
expq80value ~ cfa_populism + q81 + region + q2 + age6 + edu3n_2 + edu3n_3 + cfa_inteff

group: Left_Immigrants
cfa_populism =~ p1*q67_1 + p2*q67_2 + p3*q67_3 + p4*q67_4 + p5*q67_5
cfa_inteff =~ e1*q41_1 + e2*q41_2 + e3*q41_5
expq80value|"t5"*t1
expq80value ~ cfa_populism + q81 + region + q2 + age6 + edu3n_2 + edu3n_3 + cfa_inteff + id_str

group: Right_Immigrants
cfa_populism =~ p1*q67_1 + p2*q67_2 + p3*q67_3 + p4*q67_4 + p5*q67_5
cfa_inteff =~ e1*q41_1 + e2*q41_2 + e3*q41_5
expq80value|"t6"*t1
expq80value ~ cfa_populism + q81 + region + q2 + age6 + edu3n_2 + edu3n_3 + cfa_inteff + id_str
'

sem_lr_free <- sem(mod_pop,
                   data_clean_multigroup,
                   group = "glrcond",
                   link="probit",
                   estimator = "DWLS",
                   ordered = c("expq80value"),
                   std.lv = TRUE
)

betas_95 <- standardizedSolution(sem_lr_free, type="std.lv", level = 0.95) |> 
  filter(rhs %in% c("cfa_populism")) |> 
  filter(op %in% c("~")) |> 
  rename(ci.lower.95=ci.lower,
         ci.upper.95=ci.upper)


estimates_95 <- parameterEstimates(sem_lr_free,level = 0.95) 
estimates_90 <- parameterEstimates(sem_lr_free,level = 0.90) 

# Define a helper function to extract values for a given condition and variable
get_estimate <- function(df, rhs_value, column_name = "est") {
  # Check if rhs_value is "t1" and handle differently 
  if (rhs_value == "t1") {
    df <- df |> filter(rhs == rhs_value & op == "|")  # Use "|" for "t1"
  } else {
    df <- df |> filter(rhs == rhs_value & op == "~")  # Default for other rhs values
  }
  
  # Return the required column
  return(df |> pull({{column_name}}))
}

# Initialize output list
df_out <- list()
# Loop through unique groups
for (g in unique(estimates_95$group)) {
  
  # Filter data for the current group and extract values
  df_g <- estimates_95 |> filter(group == g)
  t <- get_estimate(df_g, "t1")
  beta <- get_estimate(df_g, "cfa_populism")
  beta_pvalue <- round(get_estimate(df_g, "cfa_populism", "pvalue"), 3)
  beta_zvalue <- round(get_estimate(df_g, "cfa_populism", "z"), 3)
  beta_ci <- df_g |> filter(rhs == "cfa_populism" & op == "~") |> select(ci.lower, ci.upper)
  
  # Extract estimates for variables q81, region, q2, edu3n_2, and edu3n_3
  variables <- c("q81", "region", "q2", "edu3n_2", "edu3n_3")
  estimates <- sapply(variables, get_estimate, df = df_g)
  
  # Calculate predicted values and confidence intervals at 95%
  pred_vals <- sapply(estimates, function(est) pnorm(-t + est) * beta)
  ci_lower_95 <- sapply(estimates, function(est) pnorm(-t + est) * beta_ci$ci.lower)
  ci_upper_95 <- sapply(estimates, function(est) pnorm(-t + est) * beta_ci$ci.upper)
  
  # Calculate means of predictions and confidence intervals
  p_mean <- round(mean(pred_vals), 3)
  p_ci.lower_95 <- round(mean(ci_lower_95), 3)
  p_ci.upper_95 <- round(mean(ci_upper_95), 3)
  
  # Repeat process for 90% confidence intervals
  df_g <- estimates_90 |> filter(group == g)
  beta_ci_90 <- df_g |> filter(rhs == "cfa_populism" & op == "~") |> select(ci.lower, ci.upper)
  ci_lower_90 <- sapply(estimates, function(est) pnorm(-t + est) * beta_ci_90$ci.lower)
  ci_upper_90 <- sapply(estimates, function(est) pnorm(-t + est) * beta_ci_90$ci.upper)
  
  # Calculate means for 90% confidence intervals
  p_ci.lower_90 <- round(mean(ci_lower_90), 3)
  p_ci.upper_90 <- round(mean(ci_upper_90), 3)
  
  # Store results in a data frame for each group
  df_out[[g]] <- data.frame(
    pp = p_mean,
    p_ci.upper_95 = p_ci.upper_95,
    p_ci.lower_95 = p_ci.lower_95,
    p_ci.lower_90 = p_ci.lower_90,
    p_ci.upper_90 = p_ci.upper_90,
    pvalue = beta_pvalue,
    zvalue = beta_zvalue,
    group = g
  )
}

# Bind rows from a list of data frames into a single data frame
plot_pp <- do.call(rbind, df_out) 

# Extract R-squared values from the SEM model and process them
r2 <- lavInspect(sem_lr_free, "rsquare")
r2_l <- purrr::map(r2, ~ .x["expq80value"]) |> unlist() |> as.array() |> round(3)
plot_pp$r2 <- r2_l

# Extract the group labels and categorize them
plot_pp$lr <- gsub("(.+?)(\\_.*)", "\\1", plot_pp$group)
plot_pp$lr <- factor(plot_pp$lr, levels = c("Left", "Center", "Right"), ordered = TRUE)
plot_pp$cond <- gsub(".*_", "", plot_pp$group)

# Format p-values for better readability in the plot
plot_pp$pvalue <- ifelse(plot_pp$pvalue < 0.001, "<= 0.001", as.character(plot_pp$pvalue))

# Create labels for the plot with statistical information
plot_pp$label <- paste0(
  "list(widehat(italic(Pr(y==1)))==", "'", plot_pp$pp, "'",
  ", italic(z)==", "'", plot_pp$zvalue, "'",
  ", italic(p)==", "'", plot_pp$pvalue, "'", 
  ", italic(r^2)==", "'", plot_pp$r2, "'",
  ")"
)

# Calculate the total sample size from the SEM model data
sample_size <- sum(unlist(sem_lr_free@Data@nobs))
label <- paste0("N: ", sample_size)

# Create the plot
plot_facet_int <- ggplot(plot_pp, aes(x = pp, y = cond, label = label)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = p_ci.lower_95, xmax = p_ci.upper_95), linetype = 1, width = 0.03, size = 0.6, colour = "grey60") +
  geom_errorbar(aes(xmin = p_ci.lower_90, xmax = p_ci.upper_90), linetype = 1, width = 0.03, size = 0.6, colour = "black") +   
  geom_label_repel(
    parse = TRUE,
    size = 2, 
    force_pull   = 0, # Do not pull toward data points
    nudge_y      = 0.05,
    direction    = "y",
    angle        = 90,
    hjust        = 0,
    segment.size = 0.2,
    max.iter = 1e4, max.time = 4
  ) +
  theme_classic() + 
  geom_vline(xintercept = 0, colour = 'grey40', linetype = "dashed", size = 0.5) + 
  facet_wrap(~ lr, ncol = 3) + 
  ggplot2::labs(
    y = "Speaker allowed to\nhold a speech against",
    x = "Marginal coefficient of populism (percentage points)"
  )

# Add annotation to the plot
plot_facet <- plot_facet_int + patchwork::plot_annotation(caption = latex2exp::TeX(label))

ggsave("figures/appendix_sem_multigroup_lridstr.png", width = 20, height = 10, units = "cm") 


