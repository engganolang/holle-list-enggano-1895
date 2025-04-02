# This is a script to merge the New Basic List in Stokhof (1980) with the Enggano List in Stokhof and Almanar (1987) via the Index match
library(tidyverse)
library(readxl)
library(qlcData)
library(stringi)

# read the NBL table from GitHub =====
holle_tb <- read_tsv("https://raw.githubusercontent.com/engganolang/digitised-holle-list/main/data/digitised-holle-list-in-stokhof-1980.tsv")

# read the Concepticon table for the NBL of the Holle List from GitHub ====
concepticon <- read_tsv("https://raw.githubusercontent.com/engganolang/digitised-holle-list/main/data/concepticon-mapping.tsv") |> 
  rename(Index = NUMBER, 
         English = GLOSS,
         Concepticon_Gloss = CONCEPTICON_GLOSS) |> 
  select(-SIMILARITY) |> 
  mutate(concept_url = paste("https://concepticon.clld.org/parameters/",
                             CONCEPTICON_ID,
                             sep = ""))
concepticon_checked <- concepticon |> 
  filter(CHECKED == "y") |> 
  select(English, Index, Concepticon_Gloss, Concepticon_ID = CONCEPTICON_ID, concept_url) |> 
  mutate(Index = as.character(Index))

# concepticon_checked_params <- concepticon_checked |> 
#   # select(-English, -Index) |> 
#   # distinct() |> 
#   mutate(Parameter_ID = paste(row_number(), "-", English, "-", Concepticon_Gloss, sep = ""))
# 
# concepticon_checked <- concepticon_checked |> 
#   left_join(concepticon_checked_params)

# read orthography profile =====
ortho_skeleton <- read_tsv("https://raw.githubusercontent.com/engganolang/enolex/refs/heads/main/ortho/_11-stockhof1987_ipa_profile.tsv") |> 
  select(-Frequency) |> 
  mutate(Class = as.character(Class)) |> 
  mutate(Class = replace_na(Class, "")) |> 
  mutate(across(matches("^Left|^Right"), ~replace_na(., ""))) |> 
  # mutate(across(where(is.character), ~replace_na(., ""))) |> 
  # editing for the orthography profiles
  add_row(tibble(Left = "", Grapheme = "q", Right = "$", Class = "", 
                 Replacement = "'", Phoneme = "ʔ")) |> 
  add_row(tibble(Left = "", Grapheme = "'", Right = "", Class = "", 
                 Replacement = "'", Phoneme = "ʔ")) |> 
  add_row(tibble(Left = "", Grapheme = " ", Right = "", Class = "", 
                 Replacement = " ", Phoneme = " ")) |> 
  mutate(Replacement = if_else(str_detect(Grapheme, stri_trans_nfc("ī")),
                               "i i",
                               Replacement),
         Phoneme = if_else(str_detect(Grapheme, stri_trans_nfc("ī")),
                           "iː",
                           Phoneme)) |> 
  add_row(tibble_row(Left = "", Grapheme = "īī", Right = "",
                     Class = "", Replacement = "i i",
                     Phoneme = "iː"), 
          .before = 1) |> 
  mutate(Phoneme = if_else(str_detect(Phoneme, stri_trans_nfc("å")),
                           "ɒ",
                           Phoneme)) |> 
  add_row(tibble_row(Left = "", Grapheme = "å", Right = "$",
                     Class = "", Replacement = "a", Phoneme = "ɯ"),
          .before = 37) |>  # in Stokhof (1980: 76, §6.2 for the discussion of "å")
  add_row(tibble_row(Left = "", Grapheme = "ng", Right = "",
                     Class = "", Replacement = "n g", Phoneme = "ŋ"),
          .before = 7) |>  # in Stokhof (1980: 77, §6.2 for the discussion of "ng")
  as.data.frame() |> 
  add_row(tibble_row(Left = "", Grapheme = "iēē", Right = "",
                     Class = "", Replacement = "i e e", Phoneme = "i eː"),
          .before = 1)
write_tsv(ortho_skeleton, "data-raw/ortho_skeleton_from_enolex.tsv")

# run the pre-processing codes to tibble the Enggano word list ====
source("code/Enggano-Holle-List.R")

# merge the NBL with the Enggano word list ====
tb <- eno_form_df_trim |> 
  left_join(holle_tb |> rename(id = Index),
            by = "id")

# determine which form row was in the original only id (e.g., <2>) without orthographical form ====
tb <- tb |> 
  mutate(form_id_only = if_else(nzchar(notes_id) & str_detect(form, "\\<", negate = TRUE),
                                TRUE,
                                FALSE))

# move the <unclear> tag to `notes_comment` ====
tb <- tb |> 
  mutate(notes_comment = if_else(str_detect(form, "\\<unclear"),
                                 str_extract(form, "(?<=\\>)([^<]+)(?=\\<\\/unclear\\>)"),
                                 notes_comment),
         form = str_replace_all(form, "\\(?\\<unclear.*?\\>", "("),
         form = str_replace_all(form, "\\<\\/unclear\\>\\)?", ")")) |> 
  # transfer notes in brackets in forms into notes eno, notes idn, and notes comment ====
  mutate(notes_idn = if_else(str_detect(form, "\\([^)]{2,}\\)") & str_detect(form, "\\(boeroek|elog|boelan gelap|lama|awan\\sawan|èkor\\)$"),
                             str_extract(form, "(boeroek|elog|boelan gelap|lama|awan\\sawan|èkor)"),
                             notes_idn),
         form = if_else(str_detect(form, "\\([^)]{2,}\\)") & str_detect(form, "\\(boeroek|elog|boelan gelap|lama|awan\\sawan|èkor\\)$"),
                        str_replace_all(form, "\\s\\((boeroek|elog|boelan gelap|lama|awan\\sawan|èkor)\\)", ""),
                        form)) |> 
  mutate(notes_eno = if_else(str_detect(form, "\\([^)]{2,}\\)") & notes_comment == "",
                             str_extract(form, "(?<=\\()[^)]+?(?=\\))"),
                             notes_eno),
         form = if_else(str_detect(form, "\\([^)]{2,}\\)") & notes_comment == "",
                        str_replace_all(form, "\\s\\(.+$", ""),
                        form)) |> 
  mutate(notes_eno = if_else(str_detect(form, "\\([^)]{2,}\\)") & notes_comment != "",
                             notes_comment,
                             notes_eno),
         notes_comment = if_else(str_detect(form, "\\([^)]{2,}\\)") & notes_comment != "",
                                 "",
                                 notes_comment),
         form = if_else(str_detect(form, "\\([^)]{2,}\\)"),
                        str_replace_all(form, "\\s\\(.+$", ""),
                        form))
# remove the notes id and tag (i.e., <1>, etc.) from the form =====
tb <- tb |> 
  mutate(form = str_replace_all(form, "\\s+<\\d+\\>", "")) |> 
  ## split multiple alternative form marked by "/" in the `form` column
  mutate(form = ifelse(str_detect(form, "\\/"),
                       str_split(form, "\\/"),
                       form)) |> 
  unnest_longer(form)

# save the rows that do not have English and Indonesian translation ===
## they are the <unsp.> in the Dutch column
# no_translation <- tb |> 
#   select(id, form, Dutch, English, Indonesian) |> 
#   filter(if_all(matches("English|Indonesian"), is.na))
# no_translation |> 
#   write_tsv('data-raw/no-translation.tsv', na = "")

# read the translated data & integrate with the main table ====
now_translated <- readxl::read_xlsx("data-raw/no-translation-now-translated.xlsx")
# tb$English[is.na(tb$English)] <- now_translated$English[which(now_translated$id %in% pull(filter(tb, is.na(English)), id))]
# tb$Indonesian[is.na(tb$Indonesian)] <- now_translated$Indonesian[which(now_translated$id %in% pull(filter(tb, is.na(Indonesian)), id))]
tb <- tb |> 
  ### integrate with the main table in code below
  left_join(now_translated |> 
              select(id, nl = Dutch, eng = English, idn = Indonesian) |> 
              distinct(), 
            by = join_by(id)) |> 
  mutate(English = if_else(is.na(English) & !is.na(eng), eng, English),
         Indonesian = if_else(is.na(Indonesian) & 
                                !is.na(idn),
                              idn, Indonesian)) |> 
  select(-nl, -eng, -idn)

tb <- tb |> 
  select(id, form, Dutch, English, Indonesian, 
         matches("notes"), everything())

# combine the note items into the comment columns =====
tb <- tb |>
  mutate(notes_comment = if_else(!nzchar(notes_comment) & nzchar(notes_id) & notes_id != "83",
                                 paste(notes_eno, 
                                       " (EN: ", 
                                       notes_eng, 
                                       "; ID: ", 
                                       notes_idn, 
                                       ") <", 
                                       notes_id, 
                                       ">", 
                                       sep = ""),
                                 notes_comment),
         notes_comment = str_replace_all(notes_comment,
                                         "(ID\\:)\\s(?=[)])",
                                         "\\1- "),
         notes_comment = str_replace_all(notes_comment,
                                         "(EN\\:)\\s(?=[;])",
                                         "\\1- "),
         notes_comment = str_trim(notes_comment,
                                  "left"),
         notes_comment = replace(notes_comment,
                                 notes_eng == "" & notes_idn == "" & notes_id != "" & notes_id != "83",
                                 ""))
## checking
tb |> 
  filter(nzchar(notes_id)) |> 
  select(id, notes_id, form, English, notes_comment) |> 
  print(n = 50)
tb

# CLDF - join the Concepticon table ====
tb <- tb |> 
  left_join(concepticon_checked |> 
              rename(id = Index),
            by = join_by(id, English))  |> 
  mutate(Parameter_ID = paste(1:nrow(tb), "-", Concepticon_Gloss, sep = ""))

## save the items with NAs Concepticon Gloss and ID =====
# tb |>
#   filter(is.na(Concepticon_Gloss)) |>
#   select(id, Dutch, English, Indonesian, matches("(^Concepticon|^concept_url)")) |> 
#   distinct() |> 
#   write_csv(file = "data-raw/NA-concepticon-gloss.csv")

# CLDF - prepare CLDF FormTable =====
## orthography conversion for Phonemes =====
forms_orthography_phoneme <- qlcData::tokenize(tb$form, 
                                       profile = ortho_skeleton, 
                                       transliterate = "Phoneme", 
                                       method = "global", 
                                       ordering = NULL, 
                                       normalize = "NFC", 
                                       regex = TRUE, 
                                       sep.replace = "+")$string |> 
  as_tibble() |> 
  rename(form = originals,
         Graphemes = tokenized,
         Segments = transliterated)

## orthography conversion for Common Orthography =====
forms_orthography_common <- qlcData::tokenize(tb$form, 
                                               profile = ortho_skeleton, 
                                               transliterate = "Replacement", 
                                               method = "global", 
                                               ordering = NULL, 
                                               normalize = "NFC", 
                                               regex = TRUE, 
                                               sep.replace = "+")$string |> 
  as_tibble() |> 
  rename(form = originals,
         Graphemes = tokenized,
         Segments_Commons = transliterated)

forms_orthography_combined <- forms_orthography_phoneme |> 
  bind_cols(forms_orthography_common |> 
              select(Segments_Commons))

### Join orthography conversion of Form into the main table =====

tb <- tb |> 
  bind_cols(forms_orthography_combined |> 
              select(-form))

## orthography conversion for Enggano forms in notes (into Phoneme) =====
eno_forms_in_notes_different_from_forms <- tb |> 
  filter(notes_eno != "", form != notes_eno) |> 
  pull(notes_eno) |> 
  unique()

forms_in_notes_orthography_phoneme <- qlcData::tokenize(eno_forms_in_notes_different_from_forms, 
                                                        profile = ortho_skeleton, 
                                                        transliterate = "Phoneme", 
                                                        method = "global", 
                                                        ordering = NULL, 
                                                        normalize = "NFC", 
                                                        regex = TRUE, 
                                                        sep.replace = "+")$string |> 
  as_tibble() |> 
  rename(notes_eno = originals,
         notes_Graphemes = tokenized,
         notes_Segments = transliterated)


## orthography conversion for Enggano forms in notes (into Common Orthography) =====
forms_in_notes_orthography_common <- qlcData::tokenize(eno_forms_in_notes_different_from_forms, 
                                                       profile = ortho_skeleton, 
                                                       transliterate = "Replacement", 
                                                       method = "global", 
                                                       ordering = NULL, 
                                                       normalize = "NFC", 
                                                       regex = TRUE, 
                                                       sep.replace = "+")$string |> 
  as_tibble() |> 
  rename(notes_eno = originals,
         notes_Graphemes = tokenized,
         notes_Segments_Commons = transliterated)

forms_in_notes_orthography_combined <- forms_in_notes_orthography_phoneme |> 
  bind_cols(forms_in_notes_orthography_common |> 
              select(notes_Segments_Commons))

### Join orthography conversion of Form-in-Notes into the main table =====

tb <- tb |> 
  left_join(forms_in_notes_orthography_combined,
            by = join_by(notes_eno)) |> 
  mutate(notes_Segments_joined = str_replace_all(notes_Segments, "\\s", "")) |> 
  # create a new notes_eno to create a new notes_comment
  mutate(notes_eno_new = notes_eno,
         notes_eno_new = if_else(!is.na(notes_Graphemes),
                                 str_c(notes_eno, " [", notes_Segments, "]", sep = ""),
                                 notes_eno_new)) |> 
  mutate(notes_comment_new = "",
         notes_comment_new = if_else(nzchar(notes_id) & notes_id != "83",
                                     paste(notes_eno_new, 
                                           " (EN: ", 
                                           notes_eng, 
                                           "; ID: ", 
                                           notes_idn, 
                                           ") <", 
                                           notes_id, 
                                           ">", 
                                           sep = ""),
                                     notes_comment),
         notes_comment_new = str_replace_all(notes_comment_new,
                                             "(ID\\:)\\s(?=[)])",
                                             "\\1- "),
         notes_comment_new = str_replace_all(notes_comment_new,
                                             "(EN\\:)\\s(?=[;])",
                                             "\\1- "),
         notes_comment_new = str_trim(notes_comment_new,
                                      "left"),
         notes_comment_new = if_else(notes_eno == form & notes_eng == "" & notes_idn == "",
                                     str_replace_all(notes_comment_new,
                                                     "^[^<]+?(?=\\<)", ""),
                                     notes_comment_new),
         notes_comment_new = if_else(notes_eno == form & notes_eng == "" & notes_idn != "" | notes_eno == form & notes_idn == "" & notes_eng != "",
                                     str_replace_all(notes_comment_new,
                                                     "^.+?(?=\\(EN)", ""),
                                     notes_comment_new),
         # notes_comment_new = replace(notes_comment_new,
         #                             notes_eng == "" & notes_idn == "" & notes_id != "" & notes_id != "83",
         #                             ""),
         notes_comment_new = if_else(notes_eno != "" & notes_id == "",
                                     paste(notes_eno_new, 
                                           " (EN: ", 
                                           notes_eng, 
                                           "; ID: ", 
                                           notes_idn, 
                                           ")", 
                                           sep = ""),
                                     notes_comment_new),
         notes_comment_new = if_else(notes_idn != "" & notes_id == "",
                                     paste(notes_eno_new, 
                                           " (EN: ", 
                                           notes_eng, 
                                           "; ID: ", 
                                           notes_idn, 
                                           ")", 
                                           sep = ""),
                                     notes_comment_new),
         notes_comment_new = str_replace_all(notes_comment_new,
                                             "(ID\\:)\\s(?=[)])",
                                             "\\1- "),
         notes_comment_new = str_replace_all(notes_comment_new,
                                             "(EN\\:)\\s(?=[;])",
                                             "\\1- "),
         notes_comment_new = str_trim(notes_comment_new,
                                      "left"))

# image files ======
img_files <- dir("img", full.names = TRUE)
img_files_tb <- tibble(img = img_files) |> 
  mutate(notes_eno = str_extract(img_files, "(?<=_)(.+)?(?=\\.png)"),
         notes_eno = str_replace(notes_eno, "\\-", " "),
         notes_id = str_extract(img_files, "26")) |> 
  mutate(ID = str_c(row_number(), "-", notes_id, sep = "")) |> 
  mutate(Media_ID = ID)
mediaTable <- img_files_tb |> 
  mutate(Description = str_c("Image file for the note ID <", notes_id, ">",
                             "; the form in the note is \"", 
                             notes_eno,
                             "\"",
                             sep = "")) |> 
  select(-notes_id, -Media_ID) |> 
  mutate(Media_Type = "image/png") |> 
  rename(Name = notes_eno) |> 
  # mutate(Name = "krandjang") |> 
  mutate(Download_URL = str_c("https://github.com/engganolang/holle-list-enggano-1895/blob/main/", img, sep = "")) |> 
  select(-img) |> 
  relocate(ID, .before = Name)


tb <- tb |> 
  left_join(mutate(select(img_files_tb, -img, -ID), notes_eno = stringi::stri_trans_nfd(notes_eno)), by = join_by(notes_id, notes_eno)) |> 
  mutate(Media_ID = replace_na(Media_ID, "")) #|> 
  #mutate(img = str_c("[", img, "](https://github.com/engganolang/holle-list-enggano-1895/blob/main/", img, ")", sep = ""),
  #       img = replace_na(img, ""))


cldf_form <- tb |> 
  rename(Form = form,
         Holle_ID = id) |> 
  mutate(ID = 1:nrow(tb),
         Language_ID = "eno1895",
         Source = "stokhof1987") |> 
  select(ID, Holle_ID, Language_ID, Parameter_ID, Form, Segments, 
         Segments_Commons, Graphemes, English, Indonesian, 
         Comment = notes_comment_new, Source, Media_ID)
write_excel_csv(cldf_form, "cldf/forms.csv")

# CLDF - save the parameters.csv (Concepticon table) ====
params_tb <- tb |> 
  select(ID = Parameter_ID, Name = English, 
         Concepticon_Gloss, Concepticon_ID) |> 
  distinct()
write_excel_csv(params_tb,
                "cldf/parameters.csv")

# CLDF - save the media.csv (MediaTable) ====
# https://github.com/cldf/cldf/blob/master/components/media/README.md
mediaTable |> 
  write_excel_csv("cldf/media.csv")

# CLDF - prepare CLDF language.csv table ====
cldf_lang <- tibble(ID = "eno1895",
                    Name = "Enggano",
                    Glottocode = "engg1245",
                    Glottolog_Name = "Enggano",
                    ISO639P3code = "eno",
                    Macroarea = "Papunesia",
                    Latitude = -5.39,
                    Longitude = 102.25,
                    Family = "Austronesian"
                    )
write_excel_csv(cldf_lang, file = "cldf/languages.csv")
