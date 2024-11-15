library(here)
source(here("scripts","data_preparation.R"))

# standardization for probit to prob. 
data_clean_multigroup <- data_clean %>%
  group_by(glrcond) %>%
  mutate(across(c(
    "age6", "q67_1", "q67_2", "q67_3", 
    "q67_4", "q67_5", "q41_1", "q41_2", "q41_5"
  ), ~ scale(., center = TRUE, scale = TRUE))) %>%
  ungroup() 


cfa_no_pop <- '

cfa_inteffa =~  "e1"*q41_1 + "e2"*q41_2 + "e3"*q41_5

expq80value ~ q81 +
  region +
  q2 +  
  age6 +  
  edu3n_2 +
  edu3n_3 +
  cfa_inteffa

'


sem_lr_free <- sem(model = cfa_no_pop,
                   data = data_clean_multigroup,
                   group = "glrcond",
                   link = "probit",
                   estimator = "DWLS",
                   ordered = c("expq80value"),
                   std.lv = TRUE
)

# Extract R-squared values from the SEM model and process them
r2 <- lavInspect(sem_lr_free, "rsquare")
r2_l_non_pop <- purrr::map(r2, ~ .x["expq80value"]) |> unlist() |> as.array() |> round(3)

cfa_pop <- '

cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
cfa_inteffa =~  "e1"*q41_1 + "e2"*q41_2 + "e3"*q41_5

expq80value ~ cfa_populism + 
  q81 +
  region +
  q2 +  
  age6 +  
  edu3n_2 +
  edu3n_3 +
  cfa_inteffa

'

sem_lr_free <- sem(model = cfa_pop,
                   data = data_clean_multigroup,
                   group = "glrcond",
                   link = "probit",
                   estimator = "DWLS",
                   ordered = c("expq80value"),
                   std.lv = TRUE
)

r2 <- lavInspect(sem_lr_free, "rsquare")
r2_l_pop <- purrr::map(r2, ~ .x["expq80value"]) |> unlist() |> as.array() |> round(3) 

r2_l_pop<- r2_l_pop[!names(r2_l_pop) %in% c("Center_Immigrants.expq80value",
                                         "Center_Multinationals.expq80value")
                                         ]

r2_l_non_pop <- r2_l_non_pop[!names(r2_l_non_pop) %in% c("Center_Immigrants.expq80value",
                                            "Center_Multinationals.expq80value")
]


mean(r2_l_pop - r2_l_non_pop)*100
