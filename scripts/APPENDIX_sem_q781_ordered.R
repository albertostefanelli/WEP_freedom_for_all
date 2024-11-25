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


mod <- '
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
cfa_inteff =~  "e1"*q41_1 + "e2"*q41_2 + "e3"*q41_5

q78_1 ~ cfa_populism + 
  q81+
  region +
  q2 +  
  age6 +  
  edu3n_2 +
  edu3n_3 +
  cfa_inteff +
  lrcses_Left +
  lrcses_Right
'


fit_1<- sem(mod,
            link="probit",
            data_clean_multigroup,
            ordered = c("q78_1d")
)

summary(fit_1, standardized=T)

dummy <- c(
  "edu3n_2",
  "edu3n_3",
  "region",
  "q2",
  "q81",
  "lrcses_Left",
  "lrcses_Right" 
)


estimates_c <- standardizedSolution(fit_1, type="std.all", level = 0.95) |> 
  filter(!rhs %in% dummy) |>
  filter(op=="~") |> 
  select(rhs, est.std, se, pvalue) |> 
  mutate_at(vars(est.std,se,pvalue), ~round(.,3)) |>
  mutate_at(vars(est.std,se,pvalue),  ~format(.,nsmall = 3)) |>
  mutate_at(vars(est.std), ~paste0(., " (",se ,")") )


estimates_dummy <- standardizedSolution(fit_1, type="std.lv", level = 0.95) |> 
  filter(rhs %in% dummy) |>
  filter(op=="~") |> 
  select(rhs, est.std, se, pvalue) |> 
  mutate_at(vars(est.std,se,pvalue), ~round(.,3)) |>
  mutate_at(vars(est.std,se,pvalue),  ~format(.,nsmall = 3)) |>
  mutate_at(vars(est.std), ~paste0(., " (",se ,")") )

estimates <- rbind(estimates_c,estimates_dummy)

merged_df <- estimates %>%
  arrange(match(rhs, c("cfa_populism", 
                       "region",
                       "q2",
                       "age6",
                       "edu3n_2",
                       "edu3n_3",
                       "q81",
                       "lrcses_Left",
                       "lrcses_Right",
                       "cfa_inteff")))




merged_df[[1]]<- recode(merged_df[[1]], 
                        `cfa_populism` = "Populist attitudes", 
                        `q81` = "Party Membership (Ref: No)",
                        `q2` = "Female (Ref: Male)",
                        `age6` = "Age",
                        `edu3n_2`= "Education: Medium (Ref: Low)",
                        `edu3n_3`= "Education: High (Ref: Low)",
                        `cfa_inteff`=  "Internal Efficacy",
                        `lrcses_Left`= "Left-wing (Ref: Centre)",
                        `lrcses_Right`= "Right-wing (Ref: Centre)",
                        `region` = "French speaking Belgium (Ref: Flanders)",
)

r2_f1 <- lavInspect(fit_1, "rsquare")["q78_1"] |> unlist() |> array() |> round(3)

merged_df[nrow(merged_df) +1, 1] <- "R^2"
merged_df[nrow(merged_df), 4] <- r2_f1
merged_df[nrow(merged_df) +1, 1] <- "Sample Size"
merged_df[nrow(merged_df), 4] <- sum(unlist(fit_1@Data@nobs))

merged_df <- merged_df |> mutate_at(vars(est.std, se, pvalue), ~ifelse(is.na(.),"", .)) |> select(-se)

border <- fp_border_default(width = 0.5)

base_line <- merged_df %>% 
  flextable() %>%
  add_header_row(values = c("", "Universal Freedom of Speech (y=1, 56%)"),
                 colwidths = c(1,2)) %>%
  align(i = 1, part = "header", align = "center") %>%
  flextable::compose(i=2, j=c(1),
                     part = "header",
                     value = as_paragraph(
                       ""
                     )
  ) |>
  flextable::compose(i=2, j=c(2),
                     part = "header",
                     value = as_paragraph(
                       "Beta"
                     )
  ) %>%
  flextable::compose(i=2, j=c(3),
                     part = "header",
                     value = as_paragraph(
                       "p-value"
                     )
  ) %>% 
  flextable::compose(i=11, j=c(1),
                     part = "body",
                     value = as_paragraph(
                       "R\U00B2"
                     )
  ) %>%
  flextable::compose(i=2, j=c(1),
                     part = "header",
                     value = as_paragraph(
                       ""
                     )
  ) %>%
  hline_bottom(border = border) %>%
  hline_top(border = border) %>%
  add_footer_lines(values = "Notes: Std. errors in parenthesis.") |>
  align(part = "footer", align = "right")


saveRDS(base_line,here("tables", "appendix_sem_q781_ordered.rds"))



