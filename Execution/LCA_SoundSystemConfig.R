library(tidyverse)
library(easylca)
library(stringr)
library(purrr)
library(glue)
library(tibble)
library(dplyr)

# ──────────────────────────────────────────────────────────────
# 1) READ + RENAME TO ≤ 8 chars
# ──────────────────────────────────────────────────────────────

d0 <- read_csv("data/lca_preprocessed_df.csv", show_col_types = FALSE) %>%
  rename(
    TS_YN   = TopSpeakerYN,
    LA_YN   = LineArrayYN,
    DEL_YN  = DelayLineYN,
    SW_ARR  = SWArrayTypes_cat,
    SW_POS  = SWArrayPosition_cat,
    CA_YN   = CenterArrayYN,
    TS_PAIR = TopSpeakerPairs,
    LA_PAIR = LineArrayPairs,
    SW_PAIR = SubWooferPairs
  ) %>%
  mutate(
    SW_ARR  = replace_na(SW_ARR, "[]"),
    SW_POS  = replace_na(SW_POS, "[]"),
    TS_PAIR = replace_na(TS_PAIR, 0L),
    LA_PAIR = replace_na(LA_PAIR, 0L),
    SW_PAIR = replace_na(SW_PAIR, 0L)
  ) %>%
  mutate(
    TS_PAIR = as.integer(TS_PAIR),
    LA_PAIR = as.integer(LA_PAIR),
    SW_PAIR = as.integer(SW_PAIR)
  )

# ──────────────────────────────────────────────────────────────
# 2) EXTRACT & SHIFT BINARY FLAGS → 1/2, RECODE SW_TYPE & SW_POS  
# ──────────────────────────────────────────────────────────────

d1 <- d0 %>%
  # these remain for some var_sets
  mutate(
    SW_BR = as.integer(str_detect(SW_ARR, "Broadside")),
    SW_GR = as.integer(str_detect(SW_ARR, "Gradient")),
    SW_EF = as.integer(str_detect(SW_ARR, "Endfire"))
  ) %>%
  # shift all 0/1 → 1/2
  mutate(across(
    c(TS_YN, LA_YN, DEL_YN, CA_YN, SW_BR, SW_GR, SW_EF),
    ~ .x + 1L
  )) %>%
  
  # collapse to 3‐level directivity (none/low/high)
  mutate(
    SW_type = case_when(
      str_detect(SW_ARR, "Gradient|Endfire") ~ 3L,  # high-directivity
      str_detect(SW_ARR, "Broadside")        ~ 2L,  # low-directivity
      TRUE                                    ~ 1L   # none
    )
  ) %>%
  
  # recode SW_POS list‐col → none/“lr”/“cen”
  mutate(
    SW_pos3 = case_when(
      SW_POS == "[]"               ~ 1L,
      str_detect(SW_POS, "lr")     ~ 2L,
      str_detect(SW_POS, "cen")    ~ 3L,
      TRUE                          ~ 1L
    )
  ) %>%

  #categorize SW_PAIR into 0 / 1-2 / 3-4
  mutate(
    SW_pcat = case_when(
      SW_PAIR == 0   ~ 1L,
      SW_PAIR %in% 1:2  ~ 2L,
      SW_PAIR %in% 3:4  ~ 3L,
      TRUE              ~ 1L
    )
  ) %>%
  
  select(
    id,
    TS_YN, LA_YN, DEL_YN,
    CA_YN,
    SW_type,        # 1=none,2=low,3=high
    SW_pos3,        # 1=none,2=lr,3=cen
    SW_BR, SW_GR, SW_EF,
    TS_PAIR, LA_PAIR, SW_PAIR,
    SW_pcat
  )

# ──────────────────────────────────────────────────────────────────
# 3)  Check Distribution & Apply Rule-of-Thumb Classification
# ──────────────────────────────────────────────────────────────────


# Check for Distribution Hints for Modeling
dist_summary <- map_dfr(
  c("TS_PAIR", "LA_PAIR", "SW_PAIR"),
  function(var) {
    x <- d1[[var]]
    tibble(
      variable = var,
      mean     = mean(x),
      var      = var(x),
      zero     = mean(x == 1),
      max      = max(x)
    )
  }
) %>%
mutate(
  classification = case_when(
    zero  > 0.5 & abs(var - mean) / mean < 0.3 ~ "Zero-inflated Poisson",
    var   > mean * 1.5 & zero <= 0.5           ~ "Negative-Binomial",
    var   > mean * 1.5 & zero >  0.5           ~ "Zero-inflated NB",
    TRUE                                      ~ "Poisson"
  )
)

print(dist_summary)

write.csv(d1, "data/LCA_Input.csv")

# ──────────────────────────────────────────────────────────────
# 4) DEFINE VAR‐SETS 
# ──────────────────────────────────────────────────────────────

var_sets <- list(
  minimal      = c("LA_YN", "DEL_YN", "SW_type", "CA_YN"),
  sw_detailed  = c("LA_YN", "SW_pcat", "DEL_YN", "SW_type", "CA_YN"),
  full_pairs   = c("TS_PAIR","LA_PAIR","SW_PAIR","DEL_YN","SW_type","CA_YN")
)

# ──────────────────────────────────────────────────────────────
# 5) AUTOMATED LCA LOOP
# ──────────────────────────────────────────────────────────────
lca_all <- imap(var_sets, function(vars, set_name) {

  # build the settings object
  if (set_name == "full_pairs") {
    settings <- define_lca(
      frame         = d1,
      analysis_name = paste0("LCA_", set_name),
      id_variable   = "id",
      nclasses      = 4,
      starts        = 250,
      cores         = 12,
      use           = vars,
      categorical   = setdiff(vars, c("TS_PAIR","LA_PAIR","SW_PAIR")),
      poisson       = c("TS_PAIR","SW_PAIR"),
      negbin = "LA_PAIR",
      inflated = "LA_PAIR"
    )
  } else {
    settings <- define_lca(
      frame         = d1,
      analysis_name = paste0("LCA_", set_name),
      id_variable   = "id",
      nclasses      = 4,
      starts        = 250,
      cores         = 12,
      use           = vars,
      categorical   = vars
    )
  }

  # **Here we explicitly pass the settings object into perform_lca()**
  res <- perform_lca(settings = settings, modeltypes = 1:4)

  # rerun any failures
  todo <- res$summary %>% filter(!replicated | is.na(Parameters))
  if (nrow(todo)) {
    res <- rerun_lca(res, recursive = TRUE)
  }

  # spot any BIC‐ties
  bests <- res$summary %>% filter(replicated) %>% slice_min(BIC, with_ties = TRUE)
  if (nrow(bests) > 1) {
    warning(glue(
      "{nrow(bests)} tied on BIC for '{set_name}':\n",
      paste0("  modeltype=", bests$modeltype,
             ", classes=",   bests$classes,
             collapse = "\n")
    ))
  }
  # break ties by fewest parameters, then highest entropy
  best <- bests %>% arrange(Parameters, desc(Entropy)) %>% slice(1)

  # pull out the chosen model
  chosen <- res$models[[paste0("modeltype_", best$modeltype)]][[ best$classes ]]

  list(
    settings  = settings,
    summary   = res$summary,
    best      = best,
    alcp      = chosen$class_counts$mostLikely,
    postprobs = chosen$individual_posterior
  )
})

# ──────────────────────────────────────────────────────────────
# 6)  ACCESS RESULTS
# ──────────────────────────────────────────────────────────────

lca_all$minimal$best        
lca_all$sw_detailed$best
lca_all$full_pairs$best

# ──────────────────────────────────────────────────────────────
# 7) COMPARISON TABLE
# ──────────────────────────────────────────────────────────────

comparison <- imap_dfr(lca_all, function(out, set_name) {

  best_any <- out$best

  tibble(
    set            = set_name,
    # global best
    any_classes    = best_any$classes,
    any_modeltype  = best_any$modeltype,
    any_BIC        = best_any$BIC,
    any_Entropy    = best_any$Entropy,
  )
})

print(comparison)
# Optionally save:
write_csv(comparison, "results/LCA_model_comparison.csv")