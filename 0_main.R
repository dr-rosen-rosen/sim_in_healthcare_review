library(here)
library(config)
library(tidyverse)
library(revtools)

debuggingState(on = FALSE)
Sys.setenv(R_CONFIG_ACTIVE = "default")
config <- config::get()
source(here("1_funcs.R"), echo = TRUE)

ref_df <- get_references()