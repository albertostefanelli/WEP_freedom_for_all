library(here)
source(here("scripts","data_preparation.R"))

data_clean_multigroup <- data_clean %>%
  group_by(glrcond) %>%
  mutate(across(c(
    "age6", "q67_1", "q67_2", "q67_3", 
    "q67_4", "q67_5", "q41_1", "q41_2", "q41_5"
  ), ~ scale(., center = TRUE, scale = TRUE))) %>%
  ungroup() %>%
  drop_na(q81, region, q2, age6, edu3n_2, edu3n_3, lrcses_Left, lrcses_Right, q41_5, q41_2, q41_1)

mod_1 <- '
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
q78_1d ~ cfa_populism
'

fit_1<- cfa(mod_1,
            data_clean_multigroup,
            link="probit",
            estimator = "DWLS",
            std.lv = TRUE,
            ordered = c("q78_1d")
)


estimates_95 <- parameterEstimates(fit_1,level = 0.95) 
t <- estimates_95[estimates_95$rhs=="t1","est"]
beta <- estimates_95[estimates_95$op=="~" ,"est"]
se <- round(estimates_95[estimates_95$op=="~" ,"se"],3)
fit1_marginal <- pnorm(-t)*beta

betas_fit_1 <- estimates_95 %>% 
  filter(op=='~') |> select(rhs, est, pvalue)

names(betas_fit_1) <- c('p','c_fit1',"pv_fit1") 

betas_fit_1$c_fit1 <- fit1_marginal
betas_fit_1$"pv_fit1" <- ifelse(betas_fit_1$"pv_fit1"<=0.001, "≤0.001", paste(" ",round(betas_fit_1$"pv_fit1",3)))

betas_fit_1 <- betas_fit_1 %>% mutate_at(vars("c_fit1"),list(~ as.numeric(.)))  %>% 
  mutate_at(vars("c_fit1"),list(~ round(.,3)))  %>% 
  mutate_at(vars("c_fit1"),list(~ format(.,3)))


mod_2 <- '

cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5

q78_1d ~ cfa_populism + 
  region +
  q2 +  
  age6 +  
  edu3n_2 +
  edu3n_3 
'

fit_2 <- cfa(mod_2,
             data_clean_multigroup,
             link="probit",
             estimator = "DWLS",
             std.lv = TRUE,
             ordered = c("q78_1d")
)


dummy <- c(
  "edu3n_2",
  "edu3n_3",
  "region",
  "q2"
)


estimates_95 <- parameterEstimates(fit_2,level = 0.95) 
dv_list <- c("q78_1d")

list_coef_out <-c()
for (d in dv_list){
  df_g <- estimates_95 |> filter(lhs==d)
  t <- df_g[df_g$rhs=="t1","est"]
  list_coef <- df_g |> filter(op=="~")
  list_coef_replace <- df_g |> filter(op=="~")
  
  for (b in 1:nrow(list_coef)){
    table_beta <- list_coef[b,]
    
    if (!table_beta$rhs %in% dummy){
      list_dummy_estimate <- list_coef |> filter(rhs%in% dummy) |> select(est)
      marginal <- pnorm(-t + unlist(list_dummy_estimate))*table_beta$est
      list_coef_replace[b,"est"]  <- mean(marginal)
    }else{
      list_dummy_estimate <- list_coef |> filter(rhs %in% dummy)  |> filter(!rhs == table_beta[1,"rhs"]) |> select(est)
      marginal <- pnorm(-t+unlist(list_dummy_estimate))*table_beta$est
      list_coef_replace[b,"est"]  <- mean(marginal)}
  }
  
  list_coef_out[[d]] <- list_coef_replace
  
  
}

binded_prob <- do.call(rbind, list_coef_out)
rownames(binded_prob)<- NULL

betas <- binded_prob |> select(lhs, rhs, est ,se, pvalue)
names(betas) <- c("dv","iv", "Coefficient","Std. Error",  "p-value")

betas <- betas %>% mutate_at(vars("p-value"),list(~ as.numeric(.)))  %>% 
  mutate_at(vars("p-value"),list(~ round(.,3)))  %>% 
  mutate_at(vars("p-value"),list(~ format(.,3)))

betas <- betas %>% mutate_at(vars("Std. Error"),list(~ as.numeric(.)))  %>% 
  mutate_at(vars("Std. Error"),list(~ round(.,3)))  %>% 
  mutate_at(vars("Std. Error"),list(~ format(.,3)))

betas <- betas %>% mutate_at(vars("Coefficient"),list(~ as.numeric(.)))  %>% 
  mutate_at(vars("Coefficient"),list(~ round(.,3)))  %>% 
  mutate_at(vars("Coefficient"),list(~ format(.,3)))

betas$"p-value" <- ifelse(betas$"p-value"<=0.001, "≤0.001", paste(" ",betas$"p-value"))

betas$Coefficient <- ifelse(betas$Coefficient<0, betas$Coefficient, paste("",betas$Coefficient))

betas <- betas %>% select(-"Std. Error")

betas_fit_2 <- betas %>% 
  filter(dv=="q78_1d") |> select(-dv)

names(betas_fit_2) <- c('p','c_fit2',"pv_fit2") 


mod_3 <- '
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
cfa_inteff =~  "e1"*q41_1 + "e2"*q41_2 + "e3"*q41_5

q78_1d ~ cfa_populism + 
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

fit_3 <- cfa(mod_3,
             data_clean_multigroup,
             link="probit",
             estimator = "DWLS",
             std.lv = TRUE,
             ordered = c("q78_1d")
)

summary(fit_3, standardized=T)

dummy <- c(
  "edu3n_2",
  "edu3n_3",
  "region",
  "q2",
  "q81",
  "lrcses_Left",
  "lrcses_Right" 
)


estimates_95 <- parameterEstimates(fit_3,level = 0.95) 
dv_list <- c("q78_1d")

pnorm(-t+ unlist(list_dummy_estimate))*table_beta$est

list_coef_out <-c()
for (d in dv_list){
  df_g <- estimates_95 |> filter(lhs==d)
  t <- df_g[df_g$rhs=="t1","est"]
  list_coef <- df_g |> filter(op=="~")
  list_coef_replace <- df_g |> filter(op=="~")
  
  for (b in 1:nrow(list_coef)){
    table_beta <- list_coef[b,]
    
    if (!table_beta$rhs %in% dummy){
      list_dummy_estimate <- list_coef |> filter(rhs%in% dummy) |> select(est)
      marginal <- pnorm(-t+ unlist(list_dummy_estimate))*table_beta$est
      list_coef_replace[b,"est"]  <- mean(marginal)
    }else{
      list_dummy_estimate <- list_coef |> filter(rhs %in% dummy)  |> filter(!rhs == table_beta[1,"rhs"]) |> select(est)
      marginal <- pnorm(-t+unlist(list_dummy_estimate))*table_beta$est
      list_coef_replace[b,"est"]  <- mean(marginal)}
  }
  
  list_coef_out[[d]] <- list_coef_replace
  
  
}

binded_prob <- do.call(rbind, list_coef_out)
rownames(binded_prob)<- NULL

betas <- binded_prob |> select(lhs, rhs, est ,se, pvalue)
names(betas) <- c("dv","iv", "Coefficient","Std. Error",  "p-value")

betas <- betas %>% mutate_at(vars("p-value"),list(~ as.numeric(.)))  %>% 
  mutate_at(vars("p-value"),list(~ round(.,3)))  %>% 
  mutate_at(vars("p-value"),list(~ format(.,3)))

betas <- betas %>% mutate_at(vars("Std. Error"),list(~ as.numeric(.)))  %>% 
  mutate_at(vars("Std. Error"),list(~ round(.,3)))  %>% 
  mutate_at(vars("Std. Error"),list(~ format(.,3)))

betas <- betas %>% mutate_at(vars("Coefficient"),list(~ as.numeric(.)))  %>% 
  mutate_at(vars("Coefficient"),list(~ round(.,3)))  %>% 
  mutate_at(vars("Coefficient"),list(~ format(.,3)))

betas$"p-value" <- ifelse(betas$"p-value"<=0.001, "≤0.001", paste(" ",betas$"p-value"))

betas$Coefficient <- ifelse(betas$Coefficient<0, betas$Coefficient, paste("",betas$Coefficient))
betas <- betas %>% select(-"Std. Error")

betas_fit_3 <- betas %>% 
  filter(dv=="q78_1d") |> select(-dv)

names(betas_fit_3) <- c('p','c_fit3',"pv_fit3") 

### TABULATING 

merged_df <- left_join(betas_fit_3,betas_fit_2, by="p")
merged_df <- left_join(merged_df,betas_fit_1, by="p")

merged_df <- merged_df %>%
  arrange(match(p, c("cfa_populism", 
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


merged_df <-  merged_df |> select(p, c_fit1, pv_fit1, c_fit2, pv_fit2,c_fit3, pv_fit3)


r2_f1 <- lavInspect(fit_1, "rsquare")["q78_1d"] |> unlist() |> array() |> round(3)
r2_f2 <- lavInspect(fit_2, "rsquare")["q78_1d"] |> unlist() |> array() |> round(3)
r2_f3 <- lavInspect(fit_3, "rsquare")["q78_1d"] |> unlist() |> array() |> round(3)


merged_df[nrow(merged_df) +1, 1] <- "R^2"
merged_df[nrow(merged_df), 3] <- r2_f1
merged_df[nrow(merged_df), 5] <- r2_f2
merged_df[nrow(merged_df), 7] <- r2_f3

merged_df[nrow(merged_df) +1, 1] <- "Sample Size"
merged_df[nrow(merged_df), 3] <- sum(unlist(fit_1@Data@nobs))
merged_df[nrow(merged_df), 5] <- sum(unlist(fit_2@Data@nobs))
merged_df[nrow(merged_df), 7] <- sum(unlist(fit_3@Data@nobs))

merged_df <-merged_df |> mutate_at(vars(c_fit1, pv_fit1, c_fit2, pv_fit2,c_fit3, pv_fit3), ~ifelse(is.na(.),"", .))


border <- fp_border_default(width = 0.5)

base_line <- merged_df %>% 
  flextable() %>%
  add_header_row(values = c("", "Universal Freedom of Speech (y=1, 56%)"),
                 colwidths = c(1,6)) %>%
  align(i = 1, part = "header", align = "center") %>%
  flextable::compose(i=2, j=c(1),
                     part = "header",
                     value = as_paragraph(
                       ""
                     )
  ) |>
  flextable::compose(i=2, j=c(2,4,6),
                     part = "header",
                     value = as_paragraph(
                       "Pr"
                     )
  ) %>%
  flextable::compose(i=2, j=c(3,5,7),
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
  #add_footer(p = paste("N= ", sample_size, ", R\U00B2= ",r2_l, sep="")) %>%
  #merge_at(j = 1:3, part = "footer") %>%
  flextable::compose(i=2, j=c(1),
                     part = "header",
                     value = as_paragraph(
                       ""
                     )
  ) %>%
  hline_bottom(border = border) %>%
  hline_top(border = border) %>%
  add_footer_lines(values = "Notes: Coefficients are not directly comparable since the variance of y* changes when new variables are added to the model.") |>
  align(part = "footer", align = "right")


saveRDS(base_line,
        here("tables", "appendix_sem_q781_exp80.rds"))


