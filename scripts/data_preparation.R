# Load the required packages
library(readr)
library(tidyverse)
library(lavaan)
library(ggplot2)
library(ggrepel)
library(emmeans)
library(here)
library(flextable)
library(gtsummary)
library(fastDummies)
library(semTools)

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

# Recode LR into categories
data_clean <- data_clean %>%
  mutate(lrcses = recode(q57, 
                         `0` = "Left", `1` = "Left", `2` = "Left", 
                         `3` = "Left", `4` = "Center", `5` = "Center", 
                         `6` = "Center", `7` = "Right", `8` = "Right", 
                         `9` = "Right", `10` = "Right"),
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
  ))

# Further data cleaning and dummy variable creation for lavaaan
data_clean <- data_clean %>%
  drop_na(edu3) %>%
  mutate(edu3n = edu3) %>%
  dummy_columns(c('edu3n')) 

# Model 2 - DV, grouping variable
data_clean <- data_clean %>%
  drop_na(expq80value, lrcses) %>%
  mutate(expq80value = as.numeric(relevel(factor(expq80value), ref = "Stopped")) - 1) %>%
  mutate(glrcond = paste(lrcses, expq80cond, sep = "_")) 

# Model 1 - DV and IV
data_clean <- data_clean %>%
    dummy_columns('lrcses') %>% 
    mutate(q78_1d = case_when(q78_1 < 3 ~ as.numeric(0), 
                              TRUE ~ as.numeric(1)))
