library(tidyverse)

# load the data
eno <- scan("Stokhof_1987_Holle_lists_Enggano.txt",
            what = "char", 
            sep = "\n",
            quiet = TRUE)

# replace tab with whitespace
eno <- gsub("\\\t", " ", eno, perl = TRUE)

# remove the dots after the ID
eno <- gsub("(?<=\\d)(\\.?\\s)", "_", eno, perl = TRUE)
head(eno, 20)
tail(eno, 20)

# get the wordlist elements -----
wlist_begin <- grep("wordlist", eno)[1]
wlist_end <- grep("wordlist", eno)[2]
eno_wlist <- eno[(wlist_begin + 1):(wlist_end - 1)]

## get the appendix A elements ----
eno_appA <- eno[(grep("appendixA", eno)[1] + 1):(grep("appendixA", eno)[2] -1)]

## get the appendix B elements ----
eno_appB <- eno[(grep("appendixB", eno)[1] + 1):(grep("appendixB", eno)[2] -1)]

## turn the wordlist and appendices into a tibble -----
### Wordlist -----
eno_wlist_df <- tibble(form = eno_wlist) |> 
  tidyr::separate(col = form,
                  into = c("id", "form"),
                  sep = "_",
                  remove = TRUE) |> 
  mutate(form_type = "wlist")
### App A -----
eno_appA_df <- tibble(form = eno_appA) |> 
  tidyr::separate(col = form,
                  into = c("id", "form"),
                  sep = "_",
                  remove = TRUE) |> 
  mutate(form_type = "appA")
### App B -----
eno_appB_df <- tibble(form = eno_appB) |> 
  tidyr::separate(col = form,
                  into = c("id", "form"),
                  sep = "_",
                  remove = TRUE) |> 
  mutate(form_type = "appB")
### Wordlist, App A, and App B combined -----
eno_form_df <- bind_rows(eno_wlist_df, eno_appA_df, eno_appB_df)

#### annotate IDs marked with "-" and "/" -----
eno_form_df <- eno_form_df |> 
  mutate(id_types = if_else(str_detect(id, "\\/"),
                            "/",
                            ""),
         id_types = if_else(str_detect(id, "\\-"),
                            "-",
                            id_types)) |> 
  # extract the notes number
  mutate(notes = str_extract(form, "(?<=\\<)\\d+(?=\\>)"))

## get the notes ----
eno_notes <- eno[(grep("\\<notes\\>", eno)[1] + 1):(grep("\\<notes\\>", eno)[2] -1)]
eno_notes_df <- tibble(form = eno_notes) |> 
  extract(form, into = c("id", "notes"), regex = "(^\\d+)_(.+$)")
