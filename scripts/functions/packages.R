#### INSTALL AND LOAD PACKAGES ==========================================================

# install pacman package if not installed -----------------------------------------------
suppressWarnings(if (!require("pacman")) install.packages("pacman"))

# load packages and install if not installed --------------------------------------------
pacman::p_load(cli, purrr, stringr, vroom, janitor, readr, vctrs, dplyr, fs, data.table,
               tibble, assertthat, gert, zoo,
               install = FALSE,
               update = FALSE)

# show loaded packages ------------------------------------------------------------------
cat("loaded packages\n")
print(pacman::p_loaded())


