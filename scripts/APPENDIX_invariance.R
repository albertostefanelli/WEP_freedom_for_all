library(here)
source(here("scripts","data_preparation.R"))

data_clean_cfa <- data_clean %>%
  drop_na(q81, region, q2, age6, edu3n_2, edu3n_3, lrcses_Left, lrcses_Right, q41_5, q41_2, q41_1)

# Manually added to Table 2
round(mean(data_clean_cfa$q67_1, na.rm=TRUE),2)
round(mean(data_clean_cfa$q67_2, na.rm=TRUE),2)
round(mean(data_clean_cfa$q67_3, na.rm=TRUE),2)
round(mean(data_clean_cfa$q67_4, na.rm=TRUE),2)
round(mean(data_clean_cfa$q67_5, na.rm=TRUE),2)

round(sd(data_clean_cfa$q67_1, na.rm=TRUE),2)
round(sd(data_clean_cfa$q67_2, na.rm=TRUE),2)
round(sd(data_clean_cfa$q67_3, na.rm=TRUE),2)
round(sd(data_clean_cfa$q67_4, na.rm=TRUE),2)
round(sd(data_clean_cfa$q67_5, na.rm=TRUE),2)


cfa_lr_cond <- 'popfs =~ q67_1 + q67_2 + q67_3 + q67_4 + q67_5'

cfa_metric <- cfa(cfa_lr_cond,
              data_clean_cfa,
              estimator="MLR",
              missing="ML",
              group.equal= c("loadings")
)
summary(cfa_metric, standardized=T)
fitmeasures(cfa_metric)


configural <- cfa(cfa_lr_cond,
                  data_clean_cfa,
                  group = "glrcond",
                  estimator="MLR",
                  missing="ML"
)

metric <- cfa(cfa_lr_cond,
              data_clean_cfa,
              group = "glrcond",
              estimator="MLR",
              missing="ML",
              group.equal= c("loadings")
)
summary(metric, standardized=T)
fitmeasures(metric)

scalar <- cfa(cfa_lr_cond,
              data_clean_cfa,
              group = "glrcond",
              estimator="MLR",
              missing="ML",
              group.equal= c("loadings", "intercepts")
)

summary(metric, standardized=TRUE)
fitmeasures(metric)


summary_fit <-compareFit(configural, metric, scalar)

sum_t <- summary(summary_fit, fit.measures=c("chisq.scaled","df", "pvalue.scaled", "cfi.robust","rmsea.robust","srmr"))

fit_cfa <- sum_t@fit

fit_cfa <- fit_cfa |> select("df", "cfi.robust","rmsea.robust","srmr") |> 
  rename(`CFI`= cfi.robust,
         `RMSEA`= rmsea.robust,
         `SRMR`= srmr,
  )


fit_cfa_diff <- sum_t@fit.diff |> 
  select("df", "cfi.robust","rmsea.robust","srmr") |>
  rename(`$\\Delta df$`= df,
         `$\\Delta CFI$`= cfi.robust,
         `$\\Delta RMSEA$`= rmsea.robust,
         `$\\Delta SRMR$`= srmr,
  )

fit_cfa_diff <- rbind(c(NA, NA, NA, NA), fit_cfa_diff)
fit_cfa_diff <-  cbind(fit_cfa, fit_cfa_diff)
fit_cfa_diff$Model <- c("Configural", "Metric", "Scalar")

fit_cfa_t <- fit_cfa_diff |> select(Model, df, `$\\Delta df$`,CFI,`$\\Delta CFI$`,RMSEA,`$\\Delta RMSEA$`,SRMR,`$\\Delta SRMR$`) 
rownames(fit_cfa_t) <- NULL
fit_cfa_t[3,5] <- abs(fit_cfa_t[3,5])

fit_cfa_t

table_invariance <- fit_cfa_t |> 
  flextable() |>
  colformat_double(j=4:9,i=1, digits = 0) |>
  colformat_double(j=4:9,i=2:3, digits = 3) |>
  flextable::compose(i=1, j=c(3),
                     part = "header",
                     value = as_paragraph(
                       "\u0394 df"
                     )
  ) |>
  flextable::compose(i=1, j=c(5),
                     part = "header",
                     value = as_paragraph(
                       "\u0394 CFI"
                     )
  ) |>
  flextable::compose(i=1, j=c(7),
                     part = "header",
                     value = as_paragraph(
                       "\u0394 RMSEA"
                     )
  ) |>
  flextable::compose(i=1, j=c(9),
                     part = "header",
                     value = as_paragraph(
                       "\u0394 SRMR"
                     )
  )


saveRDS(table_invariance, 
        here("tables", "appendix_table_invariance.rds"))


#### CFA efficacy 

cfa_lr_cond <- '
popfs =~ p1*q67_1 + p2*q67_2 + p3*q67_3 + p4*q67_4 + p5*q67_5
effs =~  l1*q41_1 + l2*q41_2 + l3*q41_5
'

metric <- cfa(cfa_lr_cond,
              data_clean_cfa,
              estimator = "ML",
              missing="ML",
)

summary(metric, standardized=TRUE)
fitMeasures(metric)
