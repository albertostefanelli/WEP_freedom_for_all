library(here)
source(here("scripts", "data_preparation.R"))

# Standardize selected variables for each group in the 'glrcond' variable.
# Scaling will center and scale these variables
data_clean_multigroup <- data_clean %>%
  group_by(glrcond) %>%
  mutate(across(c(
    "age6", "q67_1", "q67_2", "q67_3", 
    "q67_4", "q67_5", "q41_1", "q41_2", "q41_5"
  ), ~ scale(., center = TRUE, scale = TRUE))) %>%
  ungroup()

# Define CFA model without the populism factor
cfa_no_pop <- '
  cfa_inteffa =~ "e1"*q41_1 + "e2"*q41_2 + "e3"*q41_5

  # Regressions of outcome variables
  expq80value ~ q81 + region + q2 + age6 + edu3n_2 + edu3n_3 + cfa_inteffa
'

# Fit the SEM model using probit link and DWLS estimator
sem_lr_free_no_pop <- sem(model = cfa_no_pop,
                          data = data_clean_multigroup,
                          group = "glrcond",
                          link = "probit",
                          estimator = "DWLS",
                          ordered = c("expq80value"),
                          std.lv = TRUE)

# Extract R-squared values for the outcome variable (expq80value) and round them
r2_no_pop <- lavInspect(sem_lr_free_no_pop, "rsquare")
r2_l_non_pop <- purrr::map(r2_no_pop, ~ .x["expq80value"]) |> unlist() |> as.array() |> round(3)

# Define CFA model with the populism factor included
cfa_pop <- '
  cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
  cfa_inteffa =~ "e1"*q41_1 + "e2"*q41_2 + "e3"*q41_5

  # Regressions of outcome variables with additional populism factor
  expq80value ~ cfa_populism + q81 + region + q2 + age6 + edu3n_2 + edu3n_3 + cfa_inteffa
'

# Fit the SEM model with populism factor using probit link and DWLS estimator
sem_lr_free_pop <- sem(model = cfa_pop,
                       data = data_clean_multigroup,
                       group = "glrcond",
                       link = "probit",
                       estimator = "DWLS",
                       ordered = c("expq80value"),
                       std.lv = TRUE)

# Extract R-squared values for the outcome variable (expq80value) and round them
r2_pop <- lavInspect(sem_lr_free_pop, "rsquare")
r2_l_pop <- purrr::map(r2_pop, ~ .x["expq80value"]) |> unlist() |> as.array() |> round(3)

# Remove non-relevant groups (Center_Immigrants, Center_Multinationals) from R-squared results
r2_l_pop <- r2_l_pop[!names(r2_l_pop) %in% c("Center_Immigrants.expq80value", "Center_Multinationals.expq80value")]
r2_l_non_pop <- r2_l_non_pop[!names(r2_l_non_pop) %in% c("Center_Immigrants.expq80value", "Center_Multinationals.expq80value")]

# Calculate the difference in R-squared between the two models (with and without populism)
# and compute the mean percentage change in R-squared
mean_diff_r2 <- mean(r2_l_pop - r2_l_non_pop) * 100

# Print the result
mean_diff_r2
