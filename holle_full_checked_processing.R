library(tidyverse)
library(readxl)
# holle <- read_xlsx("Holle full.xlsx")
holle <- read_xlsx("Holle full-proofread.xlsx")

holle %>% pull(Enggano) %>% str_split("\\W") %>% map(function(x) x[nzchar(x)]) %>% unlist() %>% str_split("") %>% unlist() %>% table(letters = .) %>% as.data.frame() %>% filter(str_detect(letters, "[0-9]", negate = TRUE))

holle <- holle %>% 
  mutate(Enggano = str_replace_all(Enggano, "ă", "ă"),
  		 Enggano = str_replace_all(Enggano, "á", "á"),
         Enggano = str_replace_all(Enggano, "ā", "ā"),
         Enggano = str_replace_all(Enggano, "é", "é"),
         Enggano = str_replace_all(Enggano, "è", "è"),
         Enggano = str_replace_all(Enggano, "ê", "ê"),
         Enggano = str_replace_all(Enggano, "ô", "ô"),
         Enggano = str_replace_all(Enggano, "ó", "ó"))

all_chars <- holle %>% 
  pull(Enggano) %>% 
  str_split("\\W") %>% 
  map(function(x) x[nzchar(x)]) %>% 
  unlist() %>% 
  str_split("") %>% 
  unlist() %>% 
  table(letters = .) %>% 
  as.data.frame() %>% 
  filter(str_detect(letters, "[0-9]", negate = TRUE))
all_chars %>% 
  write_tsv("holle_full_checked_characters.tsv")
all_chars %>% 
  writexl::write_xlsx("holle_full_checked_characters.xlsx")

all_chars_infreq <- all_chars %>% filter(Freq <= 5)
all_chars_infreq_regex <- paste("(", paste(all_chars_infreq$letters, collapse = "|"), ")", sep = "")
all_chars_infreq_regex
holle %>% 
  filter(str_detect(Enggano, all_chars_infreq_regex))
holle %>% filter(str_detect(Enggano, "á"))
holle %>% filter(str_detect(Enggano, "ä"))
holle %>% filter(str_detect(Enggano, "é"))
holle %>% filter(str_detect(Enggano, "ò"))

holle %>% 
  mutate(across(where(is.character), function(x) str_replace_all(x, ";", ","))) %>%
  write_csv2("holle_full_checked.csv")
