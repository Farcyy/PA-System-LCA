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
  "tidyr"
))

# If you don’t have devtools/remotes yet:
install.packages("devtools") 

# Then install the package from the current directory:
# (Assuming your working directory in R is the same as the one in Terminal)
devtools::install_local(".")

# If you symlinked to /usr/local/bin/Mplus:
mp <- MplusAutomation::detectMplus()
Sys.setenv(MPLUS_CMD = mp)
message("Mplus detected at: ", mp)