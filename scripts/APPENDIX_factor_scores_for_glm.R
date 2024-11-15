# Specify the name of the dataset you want to check
dataset_name <- "data_clean"

# Check if the dataset exists
if (exists(dataset_name)) {
  
cfa_lr_cond <- '
fs_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
fs_inteff =~  "e1"*q41_1 + "e2"*q41_2 + "e3"*q41_5
'
  
metric <- cfa(cfa_lr_cond,
                data_clean,
                group = "glrcond",
                estimator = "ML",
                missing="ML"
  )

  ## Extracting FACTOR SCORES ## 
  
  idx <- lavInspect(metric, "case.idx")
  fscores <- lavPredict(metric)
  ## loop over groups and factors
  for (g in seq_along(fscores)) {
    for (fs in colnames(fscores[[g]])) {
      data_clean[ idx[[g]], fs] <- fscores[[g]][ , fs]
    }
  }
  
  
} else {
  # Print a message and end the script if the dataset does not exist
  cat("The dataset", dataset_name, "is not present in the environment. Ending the script.\n")
}

