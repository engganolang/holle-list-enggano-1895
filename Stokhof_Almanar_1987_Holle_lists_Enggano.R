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
## split notes separated by ; for each row/id -----
x <- eno_notes_df$notes |> 
  str_split("\\s;\\s")
names(x) <- paste(eno_notes_df$notes_id, "__", sep = "")
x1 <- unlist(x)
x1_df <- tibble(notes = x1, 
                notes_id = names(x1)) |> 
  mutate(notes_id = str_replace_all(notes_id, "_+(\\d+)?$", ""))
x2_df <- x1_df |> 
  mutate(notes_eno = str_extract_all(notes, "(?<=\\<eno\\>)([^<]+)(?=\\<\\/eno\\>)"),
         notes_eno_n = map_int(notes_eno, length),
         notes_idn = str_extract_all(notes, "(?<=\\<idn\\>)([^<]+)(?=\\<\\/idn\\>)"),
         notes_idn_n = map_int(notes_idn, length),
         notes_eng = str_extract_all(notes, "(?<=\\<eng\\>)([^<]+)(?=\\<\\/eng\\>)"),
         notes_eng_n = map_int(notes_eng, length),
         notes_comment = str_extract_all(notes, "(?<=\\<comment\\>)([^<]+)(?=\\<\\/comment\\>)"))
## test if there are more than one entries for each language in the notes -----
### No entries have more than one language; Good news! The comment note is only one
# x2_df |> filter(notes_eno_n>1)
# x2_df |> filter(notes_idn_n>1)
# x2_df |> filter(notes_eng_n>1)
x3_df <- x2_df |> 
  unnest(c(notes_eno, notes_idn, notes_eng, notes_comment), 
         keep_empty = TRUE) |> 
  select(-notes_eno_n,
         -notes_idn_n,
         -notes_eng_n)

# left join the notes and the forms data -----
eno_form_df <- eno_form_df |> 
  # left_join(eno_notes_df, by = "notes_id") |> 
  left_join(x3_df, by = "notes_id") |> 
  ## clean the tagging in the notes -----
  mutate(notes_clean = str_replace_all(notes, "\\<[^<]+>", ""))

eno_form_df_trim <- eno_form_df |> 
  select(-notes) |> 
  select(id, form, form_type, id_types, notes_id, notes_clean, everything()) |> 
  mutate(across(matches("notes_"), function(x) if_else(is.na(x), "", x)))

write_tsv(eno_form_df_trim,
          file = "Enggano-List-in-Stokhof-Almanar-1987.tsv")
writexl::write_xlsx(eno_form_df_trim,
                    "Enggano-List-in-Stokhof-Almanar-1987.xlsx",
                    format_headers = FALSE)
