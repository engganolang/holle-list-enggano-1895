library(tidyverse)

# load the data -----
eno <- scan("Stokhof_1987_Holle_lists_Enggano.txt",
            what = "char", 
            sep = "\n",
            quiet = TRUE)

# replace tab with whitespace -----
eno <- gsub("\\\t", " ", eno, perl = TRUE)

# remove the dots after the ID -----
eno <- gsub("(?<=\\d)(\\.?\\s+)", "_", eno, perl = TRUE)
head(eno, 20)
tail(eno, 20)

# get the wordlist and appendices elements -----
## subset the wordlist -----
wlist_begin <- grep("wordlist", eno)[1]
wlist_end <- grep("wordlist", eno)[2]
eno_wlist <- eno[(wlist_begin + 1):(wlist_end - 1)]

## subset the appendix A elements -----
eno_appA <- eno[(grep("appendixA", eno)[1] + 1):(grep("appendixA", eno)[2] -1)]

## subset the appendix B elements -----
eno_appB <- eno[(grep("appendixB", eno)[1] + 1):(grep("appendixB", eno)[2] -1)]

## subset the wordlist and appendices with multiple IDs ("/") -----
## No need to do this because these multiple IDs match the IDs in the original Basic Vocabulary List
# eno_form_multid <- c(eno_wlist, eno_appA, eno_appB) |> 
#   str_subset("(?<=\\d)\\/(?=\\d)")
### separate the form with multiple IDs to be joined in the IDs tibble (below) as a separate column -----
# forms_multid <- eno_form_multid |> 
#   str_replace_all("^[^_]+_", "")
# ids_multid <- eno_form_multid |> 
#   str_extract("^[^_]+(?=_)")
### extract each multiple IDs into a separate column in a tibble -----
# forms_multid_df <- eno_form_multid |> 
#   str_extract("^[^_]+(?=_)") |> 
#   str_extract_all("[0-9]+", 
#                   simplify = TRUE) |> 
#   data.frame() |> 
#   as_tibble() |> 
#   mutate(form = forms_multid,
#          id = ids_multid)
### make longer tibble version -----
# forms_multid_df <- forms_multid_df |> 
#   pivot_longer(cols = matches("^X"), 
#                names_to = "dummy", 
#                values_to = "ids") |> 
#   filter(nzchar(ids)) |> 
#   select(-dummy)

## turn the wordlist and appendices into a tibble -----
### wordlist -----
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
### wordlist, App A, and App B combined -----
eno_form_df <- bind_rows(eno_wlist_df, eno_appA_df, eno_appB_df)

#### annotate IDs marked with "-" and "/" -----
eno_form_df <- eno_form_df |> 
  mutate(id_types = if_else(str_detect(id, "\\/"),
                            "/",
                            ""),
         id_types = if_else(str_detect(id, "\\-"),
                            "-",
                            id_types)) |> 
#### extract the notes number -----
  mutate(notes_id = str_extract(form, "(?<=\\<)\\d+(?=\\>)"))

## join the multiple ids tibble with the eno_form_df tibble -----
## No need to do this because these multiple IDs match the IDs in the original Basic Vocabulary List 
# eno_form_df <- eno_form_df |> 
#   left_join(forms_multid_df) |> 
#   mutate(id_new = if_else(is.na(ids), id, ids))

# get the notes ----
eno_notes <- eno[(grep("\\<notes\\>", eno)[1] + 1):(grep("\\<notes\\>", eno)[2] -1)]
eno_notes_df <- tibble(form = eno_notes) |> 
  extract(form, into = c("notes_id", "notes"), regex = "(^\\d+)_(.+$)")

# left join the notes and the forms data -----
eno_form_df <- eno_form_df |> 
  left_join(eno_notes_df, by = "notes_id") |> 
  ## clean the tagging in the notes -----
  mutate(notes_clean = str_replace_all(notes, "\\<[^<]+>", ""))
