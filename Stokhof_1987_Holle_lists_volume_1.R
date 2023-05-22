# code to process the digitised Holle's list in Stokhof and Almanar (1987)
# load the package =====
library(tidyverse)
library(readxl)

holle_tb <- read_xlsx("Stokhof_1987_Holle_lists_volume_1.xlsx", sheet = 1, range = "A1:G1632")
holle_phrases_1931 <- read_xlsx("Stokhof_1987_Holle_lists_volume_1.xlsx", sheet = 2)
holle_phrases_1940_11 <- read_xlsx("Stokhof_1987_Holle_lists_volume_1.xlsx", sheet = 3)

# merging the Enggano list and the Holle list =====
source("Stokhof_1987_Holle_lists_Enggano.R")
eno_form_df_new <- eno_form_df_trim |> 
  left_join(holle_tb |> rename(id = Index),
            by = "id")