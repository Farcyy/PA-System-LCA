# Tell R which CRAN mirror to use (non‐interactive scripts need this)
options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages("BiocManager")
BiocManager::install("rhdf5")

install.packages(c(
  "tidyverse",
  "MplusAutomation",
  "patchwork",
  "purrr",
  "rlang",
  "stringr",
  "tidyr",
  "tidyverse"
))

install.packages("devtools") 
devtools::install_local(".")

# If you symlinked to /usr/local/bin/Mplus:
mp <- MplusAutomation::detectMplus()
Sys.setenv(MPLUS_CMD = mp)
message("Mplus detected at: ", mp) 