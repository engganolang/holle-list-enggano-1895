# This is a script to merge the New Basic List in Stokhof (1980) with the Enggano List in Stokhof and Almanar (1987) via the Index match
library(tidyverse)
library(readxl)

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
         form = str_replace_all(form, "\\<\\/unclear\\>\\)?", ")"))

# remove the notes id and tag (i.e., <1>, etc.) from the form =====
tb <- tb |> 
  mutate(form = str_replace_all(form, "\\s+<\\d+\\>", ""))

# save the rows that do not have English and Indonesian translation ===
## they are the <unsp.> in the Dutch column
# no_translation <- tb |> 
#   select(id, form, Dutch, English, Indonesian) |> 
#   filter(if_all(matches("English|Indonesian"), is.na))
# no_translation |> 
#   write_tsv('data-raw/no-translation.tsv', na = "")

# read the translated data & integrate with the main table ====
now_translated <- readxl::read_xlsx("data-raw/no-translation-now-translated.xlsx")
tb$English[is.na(tb$English)] <- now_translated$English[which(now_translated$id %in% pull(filter(tb, is.na(English)), id))]
tb$Indonesian[is.na(tb$Indonesian)] <- now_translated$Indonesian[which(now_translated$id %in% pull(filter(tb, is.na(Indonesian)), id))]

tb <- tb |> 
  select(id, form, Dutch, English, Indonesian, 
         matches("notes"), everything())

# combine the note items into the comment columns =====
tb <- tb |>
  mutate(notes_comment = if_else(!nzchar(notes_comment) & nzchar(notes_id),
                                 paste(notes_eno, " (EN: ", notes_eng, "; ID: ", notes_idn, ") <", notes_id, ">", sep = ""),
                                 notes_comment),
         notes_comment = str_replace_all(notes_comment,
                                         "(ID\\:)\\s(?=[)])",
                                         "\\1- "),
         notes_comment = str_replace_all(notes_comment,
                                         "(EN\\:)\\s(?=[;])",
                                         "\\1- "),
         notes_comment = str_trim(notes_comment,
                                  "left"))
## checking
tb |> 
  filter(nzchar(notes_id)) |> 
  select(id, notes_id, form, English, notes_comment) |> 
  print(n = 50)
tb

# CLDF - join the Concepticon table ====
tb <- tb |> 
  left_join(concepticon_checked |> 
              rename(id = Index)) |> 
  mutate(Parameter_ID = paste(1:nrow(tb), "-", Concepticon_Gloss, sep = ""))

## save the items with NAs Concepticon Gloss and ID =====
# tb |>
#   filter(is.na(Concepticon_Gloss)) |>
#   write_csv(file = "data-raw/NA-concepticon-gloss.csv")

# CLDF - prepare CLDF FormTable =====
cldf_form <- tb |> 
  rename(Form = form,
         Holle_ID = id) |> 
  mutate(ID = 1:nrow(tb),
         Language_ID = "eno1895",
         Source = "stokhof1987") |> 
  select(ID, Holle_ID, Language_ID, Parameter_ID, Form, English, Indonesian, Comment = notes_comment, Source)
write_excel_csv(cldf_form, "cldf/forms.csv")

# CLDF - save the parameters.csv (Concepticon table) ====
write_excel_csv(select(tb, ID = Parameter_ID, Name = English, Concepticon_Gloss, Concepticon_ID),
                "cldf/parameters.csv")

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
