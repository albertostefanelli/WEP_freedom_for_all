library(here)
source(here("scripts","data_preparation.R"))

# standardization for probit to prob. 
data_clean_multigroup <- data_clean %>%
  group_by(glrcond) %>%
  mutate(across(c(
    "age6", "q67_1", "q67_2", "q67_3", 
    "q67_4", "q67_5", "q41_1", "q41_2", "q41_5"
  ), ~ scale(., center = TRUE, scale = TRUE))) %>%
  ungroup() %>%
  drop_na(q81, region, q2, age6, edu3n_2, edu3n_3, lrcses_Left, lrcses_Right, q41_5, q41_2, q41_1)

mod_1 <- '

group: Right_Multinationals 
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
expq80value|"t1"*t1
expq80value ~ b1g1*cfa_populism 

group: Center_Multinationals 
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
expq80value|"t2"*t1
expq80value ~ b1g2*cfa_populism 

group: Center_Immigrants 
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
expq80value|"t4"*t1
expq80value ~ b1g4*cfa_populism 

group: Left_Multinationals 
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
expq80value|"t3"*t1
expq80value ~ b1g3*cfa_populism

group: Left_Immigrants 
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
expq80value|"t5"*t1
expq80value ~ b1g5*cfa_populism 

group: Right_Immigrants 
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
expq80value|"t6"*t1
expq80value ~ b1g6*cfa_populism

'

fit_1 <- sem(mod_1,
             data_clean_multigroup,
             group = "glrcond",
             link="probit",
             estimator = "DWLS",
             std.lv=TRUE,
             ordered = c("expq80value")
)

summary(fit_1)

estimates_95 <- parameterEstimates(fit_1,level = 0.95) 
t <- estimates_95[estimates_95$rhs=="t1","est"]
beta <- estimates_95[estimates_95$op=="~" ,"est"]
se <- round(estimates_95[estimates_95$op=="~" ,"se"],3)
fit1_marginal <- pnorm(-t)*beta

betas_fit_1 <- estimates_95 %>% 
  filter(op=='~') |> select(rhs, est, pvalue, group)

names(betas_fit_1) <- c('p','c_fit1',"pv_fit1", "group") 

betas_fit_1$c_fit1 <- fit1_marginal
betas_fit_1$"pv_fit1" <- ifelse(betas_fit_1$"pv_fit1"<=0.001, "≤0.001", paste(" ",round(betas_fit_1$"pv_fit1",3)))

betas_fit_1 <- betas_fit_1 %>% mutate_at(vars("c_fit1"),list(~ as.numeric(.)))  %>% 
  mutate_at(vars("c_fit1"),list(~ round(.,3)))  %>% 
  mutate_at(vars("c_fit1"),list(~ format(.,3)))

betas_fit_1$model <- "fit_1"

mod_2 <- '

group: Right_Multinationals 
expq80value|"t1"*t1
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
expq80value ~ cfa_populism +
  region +
  q2 +  
  age6 +  
  edu3n_2 +
  edu3n_3

group: Center_Multinationals 
expq80value|"t2"*t1
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
expq80value ~ cfa_populism +
  region +
  q2 +  
  age6 +  
  edu3n_2 +
  edu3n_3

group: Left_Multinationals 
expq80value|"t3"*t1
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
expq80value ~ cfa_populism +
  region +
  q2 +  
  age6 +  
  edu3n_2 +
  edu3n_3

group: Center_Immigrants 
expq80value|"t4"*t1
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
expq80value ~ cfa_populism +
  region +
  q2 +  
  age6 +  
  edu3n_2 +
  edu3n_3

group: Left_Immigrants 
expq80value|"t5"*t1
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
expq80value ~ b1g5*cfa_populism +
  region +
  q2 +  
  age6 +  
  edu3n_2 +
  edu3n_3


group: Right_Immigrants 
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
expq80value|"t6"*t1
expq80value ~ cfa_populism +
  region +
  q2 +  
  age6 +  
  edu3n_2 +
  edu3n_3


'

fit_2 <- sem(mod_2,
             data_clean_multigroup,
             group = "glrcond",
             link="probit",
             estimator = "DWLS",
             std.lv=TRUE,
             ordered = c("expq80value")
)


dummy <- c(
  "edu3n_2",
  "edu3n_3",
  "region",
  "q2",
  "q81"
)

estimates_95 <- parameterEstimates(fit_2,level = 0.95) 

list_coef_out <-c()
for (d in unique(estimates_95$group)){
  df_g <- estimates_95 |> filter(group==d)
  t <- df_g[df_g$rhs=="t1","est"]
  list_coef <- df_g |> filter(op=="~")
  list_coef_replace <- df_g |> filter(op=="~")
  
  for (b in 1:nrow(list_coef)){
    table_beta <- list_coef[b,]
    
    if (!table_beta$rhs %in% dummy){
      list_dummy_estimate <- list_coef |> filter(rhs%in% dummy) |> select(est)
      marginal <- pnorm(-t+unlist(list_dummy_estimate))*table_beta$est
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

betas <- binded_prob |> select(lhs, rhs, est ,se, pvalue, group)
names(betas) <- c("dv","iv", "Coefficient","Std. Error",  "p-value", "group")

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

betas <- betas |> select(-dv)
betas_fit_2 <- betas |> mutate(model="fit_2")


mod_3 <- '

group: Right_Multinationals 
expq80value|"t1"*t1
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
cfa_inteff =~  "e1"*q41_1 + "e2"*q41_2 + "e3"*q41_5
expq80value ~ cfa_populism +
  region +
  q2 +  
  age6 +  
  edu3n_2 +
  edu3n_3 +
  q81 +
  cfa_inteff 

group: Center_Multinationals 
expq80value|"t2"*t1
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
cfa_inteff =~  "e1"*q41_1 + "e2"*q41_2 + "e3"*q41_5
expq80value ~ cfa_populism +
  region +
  q2 +  
  age6 +  
  edu3n_2 +
  edu3n_3 +
  q81 +
  cfa_inteff 

group: Left_Multinationals 
expq80value|"t3"*t1
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
cfa_inteff =~  "e1"*q41_1 + "e2"*q41_2 + "e3"*q41_5
expq80value ~ b1g3*cfa_populism +
  region +
  q2 +  
  age6 +  
  edu3n_2 +
  edu3n_3 +
  q81 +
  cfa_inteff 

group: Center_Immigrants 
expq80value|"t4"*t1
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
cfa_inteff =~  "e1"*q41_1 + "e2"*q41_2 + "e3"*q41_5
expq80value ~ cfa_populism +
  region +
  q2 +  
  age6 +  
  edu3n_2 +
  edu3n_3 +
  q81 +
  cfa_inteff 

group: Left_Immigrants 
expq80value|"t5"*t1
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
cfa_inteff =~  "e1"*q41_1 + "e2"*q41_2 + "e3"*q41_5
expq80value ~ cfa_populism +
  region +
  q2 +  
  age6 +  
  edu3n_2 +
  edu3n_3 +
  q81 +
  cfa_inteff 


group: Right_Immigrants 
cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
cfa_inteff =~  "e1"*q41_1 + "e2"*q41_2 + "e3"*q41_5
expq80value|"t6"*t1
expq80value ~ b1g6*cfa_populism +
  b4g6*region +
  b5g6*q2 +  
  b6g6*age6 +  
  b71g6*edu3n_2 +
  b72g6*edu3n_3 +
  b8g6*q81 +
  b7g6*cfa_inteff 


'

fit_3 <- sem(mod_3,
             data_clean_multigroup,
             group = "glrcond",
             link="probit",
             estimator = "DWLS",
             ordered = c("expq80value"),
             std.lv=TRUE
)


summary(fit_3, standardized=T)

dummy <- c(
  "edu3n_2",
  "edu3n_3",
  "region",
  "q2",
  "q81"
)

estimates_95 <- parameterEstimates(fit_3,level = 0.95) 

list_coef_out <-c()
for (d in unique(estimates_95$group)){
  #d <- "Right_Multinationals"
  df_g <- estimates_95 |> filter(group==d)
  t <- df_g[df_g$rhs=="t1","est"]
  list_coef <- df_g |> filter(op=="~")
  list_coef_replace <- df_g |> filter(op=="~")
  
  for (b in 1:nrow(list_coef)){
    b <- 1
    table_beta <- list_coef[b,]
    
    if (!table_beta$rhs %in% dummy){
      list_dummy_estimate <- list_coef |> filter(rhs %in% dummy) |> select(est)
      marginal <- pnorm(-t + unlist(list_dummy_estimate))*table_beta$est
      list_coef_replace[b,"est"]  <- mean(marginal)
    }else{
      list_dummy_estimate <- list_coef |> filter(rhs %in% dummy)  |> filter(!rhs == table_beta[1,"rhs"]) |> select(est)
      marginal <- pnorm(-t + unlist(list_dummy_estimate))*table_beta$est
      list_coef_replace[b,"est"]  <- mean(marginal)}
  }
  
  list_coef_out[[d]] <- list_coef_replace
  
  
}

binded_prob <- do.call(rbind, list_coef_out)
rownames(binded_prob)<- NULL

betas <- binded_prob |> select(lhs, rhs, est ,se, pvalue, group)
names(betas) <- c("dv","iv", "Coefficient","Std. Error",  "p-value", "group")

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

betas <- betas |> select(-dv)
betas_fit_3 <- betas |> mutate(model="fit_3")

table <- c()
for (g in unique(betas_fit_3$group)){
  betas_gf1 <- betas_fit_1 |> filter(group==g) |> select(-group, -model)
  names(betas_gf1) <- c('p','c_fit1',"pv_fit1")
  betas_gf2 <- betas_fit_2 |> filter(group==g) |> select(-group, -model)
  names(betas_gf2) <- c('p','c_fit2',"pv_fit2")
  betas_gf3 <- betas_fit_3 |> filter(group==g) |> select(-group, -model)
  names(betas_gf3) <- c('p','c_fit3',"pv_fit3")
  
  merged_df <- left_join(betas_gf3,betas_gf2, by="p")
  merged_df <- left_join(merged_df,betas_gf1, by="p")
  
  merged_df$p <- recode(merged_df$p, 
                        `cfa_populism` = "Populist attitudes", 
                        `q81` = "Party membership (Ref: No)",
                        `q2` = "Female (Ref: Male)",
                        `age6` = "Age",
                        `edu3n_2`= "Education: Medium (Ref: Low)",
                        `edu3n_3`= "Education: High (Ref: Low)",
                        `cfa_inteff`=  "Internal efficacy",
                        `region` = "French speaking Belgium (Ref: Flanders)")
  
  merged_df <-  merged_df |> select(p, c_fit1, pv_fit1, c_fit2, pv_fit2,c_fit3, pv_fit3)
  
  r2_f1 <- lavInspect(fit_1, "rsquare")[[g]] %>% .[["expq80value"]] |> unlist() |> array() |> round(3)
  r2_f2 <- lavInspect(fit_2, "rsquare")[[g]] %>% .[["expq80value"]] |> unlist() |> array() |> round(3)
  r2_f3 <- lavInspect(fit_3, "rsquare")[[g]] %>% .[["expq80value"]] |> unlist() |> array() |> round(3)
  
  
  merged_df[nrow(merged_df)+1, 1] <- "R^2"
  merged_df[nrow(merged_df), 3] <- r2_f1
  merged_df[nrow(merged_df), 5] <- r2_f2
  merged_df[nrow(merged_df), 7] <- r2_f3
  
  merged_df[nrow(merged_df)+1, 1] <- "Sample Size"
  merged_df[nrow(merged_df), 3] <- sum(unlist(fit_1@Data@nobs))
  merged_df[nrow(merged_df), 5] <- sum(unlist(fit_2@Data@nobs))
  merged_df[nrow(merged_df), 7] <- sum(unlist(fit_3@Data@nobs))
  
  merged_df <-merged_df |> mutate_at(vars(c_fit1, pv_fit1,c_fit2,pv_fit2), ~ifelse(is.na(.),"", .))
  
  table[[g]] <- merged_df 
  
  
}

border <- fp_border_default(width = 0.5)

for (t in unique(names(table))){
  
  base_line <- table[[t]] %>% 
    flextable() %>%
    add_header_row(values = c("", "Conditional Freedom of Speech (Split-ballot)"),
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
    flextable::compose(i=9, j=c(1),
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
  
  
  saveRDS(base_line,here("tables", paste0("appendix_sem_multigroup_", t,".rds") ))
  
  
}
