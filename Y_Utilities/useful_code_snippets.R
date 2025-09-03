#########################################################
# Random useful code snippets
#
#########################################################


#-----------------------------------------------------------------------
# Installing packages and loading from custom location
#-----------------------------------------------------------------------

# 1) Pick a personal library path (under your $HOME)
#user_lib <- file.path(Sys.getenv("HOME"), "R", paste(R.version$major, R.version$minor, sep = "."))
#user_lib <- file.path(h, "/repo/Aim2/Y_Utilities/R_Packages/")
dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)

# 2) Prepend it to the library search path
.libPaths(c(user_lib, .libPaths()))

# 3) Install ggpol into your user lib
install.packages("ggpol", lib = user_lib, repos = "https://cran.r-project.org")
install.packages("tidycensus", lib = user_lib, repos = "https://cran.r-project.org")

# 4) Load it (works because user_lib is in .libPaths())
library(ggpol)
library(tidycensus)
