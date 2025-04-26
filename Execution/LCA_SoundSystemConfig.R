library(tidyverse)
library(easylca)      # <-- and your LCA functions

# ─── Read LCA preprocessed CSV ───

rm(list = ls())  # clear environment
data <- read_csv("lca_preprocessed_df.csv")

data_prepro <- data %>% rename(
  LA_YN   = LineArrayYN,
  DEL_YN   = DelayLineYN,
  ARR_BRD  = ARR_BROADSIDE,
  ARR_GRD   = ARR_GRAD,
  ARR_END   = ARR_END,
  ARR_LR    = ARR_LR,
  ARR_CEN   = ARR_CEN
)

data_prepro$SW_YN <- NULL
data_prepro$TS_YN <- NULL

# ─── Define LCA settings ───

lca_settings <- define_lca(
  data_prepro,
  "testanalyse",   # a name/stem for output files & plots
  "id",            # the column in `data` that uniquely identifies each case
  cores = 12,      # use up to 12 CPU cores for parallel estimation
  starts = 1000, 100,     # number of random‐start optimizations per model
  #cat = c("var1", "var2"), # treat as categorical manifest indicators
  #censored_b = "var8", # treat as a censored (bottom‐censored) variable
  #inflated = "var8" # model `with a point‐mass (zero‐inflated) component
)

# ─── Fit 1–4‐class models ───
lca_results <- perform_lca(lca_settings, modeltypes = seq(3))

# ─── Rerun best seeds to avoid local optima ───
lca_results <- rerun_lca(lca_results)

# ─── Model selection plot (AIC, BIC, entropy…) ───
lca_results$plots$summaryw

# ─── Profile plot for the 4‐class solution ───
lca_results$plots$modeltype_3[[2]]

# ─── Extract each case’s assigned class & posterior probs ───
classifications <- lca_results$models$modeltype_3[[2]]$savedata