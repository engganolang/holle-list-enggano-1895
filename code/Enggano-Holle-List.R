# This is the code to process the Enggano word list into a tibble/data frame and tab-separated file
# There will be a different code to match the ID of this Enggano word list with the main Holle List
library(tidyverse)
# library(writexl)

# Load the data -----
eno <- scan("data-raw/Enggano-Holle-List.txt",
            what = "char", 
            sep = "\n",
            quiet = TRUE)

# Replace tab with whitespace -----
eno <- gsub("\\\t", " ", eno, perl = TRUE)

# Remove the dots after the ID -----
eno <- gsub("(?<=\\d)(\\.?\\s+)", "_", eno, perl = TRUE)
head(eno, 20)
tail(eno, 20)

# Get the wordlist and appendices elements -----
## Subset the wordlist -----
wlist_begin <- grep("wordlist", eno)[1]
wlist_end <- grep("wordlist", eno)[2]
eno_wlist <- eno[(wlist_begin + 1):(wlist_end - 1)]

## Subset the appendix A elements -----
eno_appA <- eno[(grep("appendixA", eno)[1] + 1):(grep("appendixA", eno)[2] -1)]

## Subset the appendix B elements -----
eno_appB <- eno[(grep("appendixB", eno)[1] + 1):(grep("appendixB", eno)[2] -1)]

## Subset the wordlist and appendices with multiple IDs ("/") -----
## No need to do this because these multiple IDs match the IDs in the original Basic Vocabulary List (Stokhof 1980)
# eno_form_multid <- c(eno_wlist, eno_appA, eno_appB) |> 
#   str_subset("(?<=\\d)\\/(?=\\d)")
### Separate the form with multiple IDs to be joined in the IDs tibble (below) as a separate column -----
# forms_multid <- eno_form_multid |> 
#   str_replace_all("^[^_]+_", "")
# ids_multid <- eno_form_multid |> 
#   str_extract("^[^_]+(?=_)")
### Extract each multiple IDs into a separate column in a Tibble -----
# forms_multid_df <- eno_form_multid |> 
#   str_extract("^[^_]+(?=_)") |> 
#   str_extract_all("[0-9]+", 
#                   simplify = TRUE) |> 
#   data.frame() |> 
#   as_tibble() |> 
#   mutate(form = forms_multid,
#          id = ids_multid)
### Make a longer tibble version -----
# forms_multid_df <- forms_multid_df |> 
#   pivot_longer(cols = matches("^X"), 
#                names_to = "dummy", 
#                values_to = "ids") |> 
#   filter(nzchar(ids)) |> 
#   select(-dummy)

## Turn the wordlist and appendices into a tibble -----
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
### Wordlist, App A, and App B combined -----
eno_form_df <- bind_rows(eno_wlist_df, eno_appA_df, eno_appB_df) |> 
  
  ## spliting multiple forms in a cell
  mutate(form = ifelse(str_detect(form, "\\,"), 
                       str_split(form, "\\s?\\,\\s"), 
                       form)) |> 
  unnest_longer(form)

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

## Join the multiple ids tibble with the eno_form_df Tibble -----
## No need to do this because these multiple IDs match the IDs in the original Basic Vocabulary List 
# eno_form_df <- eno_form_df |> 
#   left_join(forms_multid_df) |> 
#   mutate(id_new = if_else(is.na(ids), id, ids))

# Get the notes ----
eno_notes <- eno[(grep("\\<notes\\>", eno)[1] + 1):(grep("\\<notes\\>", eno)[2] -1)]
eno_notes_df <- tibble(form = eno_notes) |> 
  extract(form, into = c("notes_id", "notes"), regex = "(^\\d+)_(.+$)")
## Split notes separated by ; for each row/id -----
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
## Test if there is more than one entry for each language in the notes -----
### No entries have more than one language; Good news! The comment note is only one
# x2_df |> filter(notes_eno_n>1)
# x2_df |> filter(notes_idn_n>1)
# x2_df |> filter(notes_eng_n>1)
x3_df <- x2_df |> 
  unnest(c(notes_eno, notes_idn, notes_eng, notes_comment), 
         keep_empty = TRUE) |> 
  select(-notes_eno_n,
         -notes_idn_n,
         -notes_eng_n) |> 
  mutate(notes_eno = ifelse(str_detect(notes_eno, ","), 
                            str_split(notes_eno, "\\s?\\,\\s"), 
                            notes_eno)) |> 
  unnest_longer(notes_eno)

# Left join the notes and the forms data -----
eno_form_df <- eno_form_df |> 
  # left_join(eno_notes_df, by = "notes_id") |> 
  left_join(x3_df, by = "notes_id", relationship = "many-to-many") |> 
  ## clean the tagging in the notes -----
  mutate(notes_clean = str_replace_all(notes, "\\<[^<]+>", ""))

eno_form_df_trim <- eno_form_df |> 
  select(-notes) |> # remove the original notes containing the tags
  select(id, form, form_type, id_types, notes_id, notes_clean, everything()) |> 
  mutate(across(matches("notes_"), function(x) if_else(is.na(x), "", x)))

# replace the note-ID only in the form column with the values from the notes_eno
eno_form_df_trim <- eno_form_df_trim |> 
  select(-notes_clean) |>  # remove the cleaned notes column
  distinct() |> 
  mutate(form = if_else(str_detect(form, "^<"), 
                        notes_eno, 
                        form))

# Save the Enggano list into a tab-separated file -----
# write_tsv(eno_form_df_trim, file = "Enggano-List-in-Stokhof-Almanar-1987.tsv")
# writexl::write_xlsx(eno_form_df_trim, "Enggano-List-in-Stokhof-Almanar-1987.xlsx", format_headers = FALSE)
