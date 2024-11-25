# Load the base data
base_data <- readr::read_csv2(here::here("data", "bnes_wep.csv"))

# Clean the data
data_clean <- base_data %>%
  mutate(across(c(q78_1, q41_1, q41_2, q41_5, q67_1, q67_2, q67_3, q67_4, q67_5, q40_3, q75_7, q49_1, q49_2, q49_3, q49_4), 
                ~ case_when(. > 5 ~ NA_real_, TRUE ~ as.numeric(.)))) %>%
  mutate(across(c(q81), 
                ~ case_when(. > 1 ~ NA_real_, TRUE ~ as.numeric(.)))) %>%
  mutate(across(c(q57), 
                ~ case_when(. > 10 ~ NA_real_, TRUE ~ as.numeric(.)))) %>%
  mutate(across(c(expq80value), 
                ~ case_when(. == "no_response" ~ NA_character_ , TRUE ~ .))) %>%
  mutate(across(c(q2, region), 
                ~ . - 1))


sum(is.na(base_data$q78_1))
table(base_data$q78_1)

# Recode LR into categories
data_clean <- data_clean %>%
  mutate(lrcses = recode(q57, 
                         `0` = "Left", `1` = "Left", `2` = "Left", 
                         `3` = "Left", `4` = "Center", `5` = "Center", 
                         `6` = "Center", `7` = "Right", `8` = "Right", 
                         `9` = "Right", `10` = "Right") %>% factor(levels = c("Left", "Center", "Right"), ordered = TRUE),
         id_str = recode(q57, 
                         `3`= 1,
                         `2`= 2,
                         `1`= 3,
                         `0`= 4,
                         `7`= 1,
                         `8`= 2,
                         `9`= 3,
                         `10`= 4, 
                         `4`= 0,
                         `5`= 0,
                         `6`= 0 
         )) %>%
  mutate(q78_1d = case_when(q78_1 <= 3 ~ as.numeric(0), 
                            q78_1 > 3 ~ as.numeric(1),
                            TRUE ~ NA_integer_))



data_clean_des <- data_clean %>%
mutate(
  edu3 = case_when(
    edu3 == 1 ~ "Low",
    edu3 == 2 ~ "Medium",
    edu3 == 3 ~ "High"
  ) %>% factor(levels = c("Low", "Medium", "High"), ordered = TRUE),
  q78_1d = case_when(
    q78_1d == 0 ~ "Not in favour",
    q78_1d == 1 ~ "In favour",
), 
  q81 = case_when(
    q81 == 0 ~ "No ID",
    q81 == 1 ~ "Yes"
  ),
region = case_when(
  region == 0 ~ "Flanders",
  region == 1 ~ "French-speaking Belgium"
),
q2 = case_when(
  q2 == 0 ~ "Male",
  q2 == 1 ~ "Female"
)
) %>%
  select(-q57)

descriptives_variables <- c(
  "q78_1d",
  "expq80value",
  "expq80cond",
  "q2",
  "edu3",
  "age6",
  "lrcses",
  "q81",
  "region",
  "q41_1",
  "q41_2",
  "q41_5",
  "q67_1",
  "q67_2",
  "q67_3",
  "q67_4",
  "q67_5",
  "q78_1",
  "id_str",
  "q75_7",
  "q40_3",
  "q49_1",
  "q49_2",
  "q49_3",
  "q49_4"
)

data_clean_des <- data_clean_des %>% select(descriptives_variables)

dummy <- c("q78_1d",
           "expq80value",
           "expq80cond",
           "q81",
           "q2",
           "lrcses",
           "region",
           "edu3")


table_desc <- 
  tbl_summary(
    data_clean_des,
    type = list(names(data_clean_des)[!names(data_clean_des) %in% c(dummy)] ~ 'continuous2'),
    missing = "no", # don't list missing data separately
    statistic = list(names(data_clean_des)[!names(data_clean_des) %in% c(dummy)] ~ c("{mean} ({sd})","{median} ({p25}, {p75})", "{min} - {max}"),
                     dummy ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,
    label = c(
      q78_1d ~ "Unconstrained free speech (Normative, q78_1d)",
      expq80value ~ "Ideologically-oriented speech (Instrumental, DV Experiment, q80)",
      expq80cond ~ "Speech target (split-ballot condition, expq80cond)",
      q81 ~ "Identify with any political party (q81)",
      q2 ~ "Sex assigned at birth (q2)",
      age6 ~ "Age (age6)",
      lrcses ~ "Left-Right self-placement (q57)",
      region ~ "Region of residence (region)",
      edu3 ~ "Educational level (edu3)",
      q41_1 ~ "Internal efficacy (q41_1)",
      q41_2 ~ "Internal efficacy (q41_2)",
      q41_5 ~ "Internal efficacy (q41_5)",
      q67_1 ~ "Populism (q67_1)",
      q67_2 ~ "Populism (q67_2)",
      q67_3 ~ "Populism (q67_3)",
      q67_4 ~ "Populism (q67_4)",
      q67_5 ~ "Populism (q67_5)",
      
      q78_1 ~  "Abstract free speech (Robustness, continuous q78_1)",
      id_str ~ "Ideological Extremity (Robustness, folded q57)",
      
      q75_7 ~ "Egalitarianism (Robustness, q75_7)",
      q40_3 ~ "Egalitarianism (Robustness, q40_3)",
      
      q49_1 ~ "Anti-immigration (Robustness, q49_1)", 
      q49_2 ~ "Anti-immigration (Robustness, q49_2)", 
      q49_3 ~ "Anti-immigration (Robustness, q49_3)", 
      q49_4 ~ "Anti-immigration (Robustness, q49_4)" 
    )) %>%
  add_n(statistic = "{n_miss} ({p_miss}%)") %>% # add column with total number of non-missing observations
  modify_header(label = "**Variable**", n = "**Missing**") %>% # update the column header
  bold_labels()


saveRDS(table_desc, here("tables", "appendix_descriptives.rds"))

