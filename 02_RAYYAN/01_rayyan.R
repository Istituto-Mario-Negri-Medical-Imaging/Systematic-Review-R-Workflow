# PULIZIA E LIBRERIE ####
rm(list = ls())
setwd("../02_RAYYAN")
source("../01_IMPORT/Libraries.R")
load("../01_IMPORT/.RData")
rm(list = setdiff(ls(), c("bib", "bib_easy", "counts")))

# RIMOZIONE DUPLICATI TROVATI CON RAYYAN ####
rows_to_remove <- c(
  "MASCHMEYER I, 2015, EUR J PHARM BIOPHARM-a",
  "SATOH T, 2018, LAB CHIP",
  "WANG Y, 2024, LAB CHIP",
  "KANABEKOVA P, 2023, LAB CHIP",
  "NAIRON K, 2025, BIOFABRICATION",
  "HASSELL B, 2017, CELL REP",
  "KIMURA H, 2023, LAB CHIP",
  "VIOLA H, 2023, LAB CHIP",
  "COGNETTI J, 2022, LAB CHIP"
)

bib_easy <- bib_easy[!rownames(bib_easy) %in% rows_to_remove, ]
bib <- bib[!rownames(bib) %in% rows_to_remove, ]

# FUNZIONI UTILI ####
save_table_to_excel <- function(data, sheet_name, file_name = "Output_02.xlsx") {
  wb <- if (file.exists(file_name)) loadWorkbook(file_name) else createWorkbook()
  if (!(sheet_name %in% names(wb))) addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, x = data)
  saveWorkbook(wb, file_name, overwrite = TRUE)
}

prepare_bib_table <- function(df) {
  df_out <- df[, c("TI_raw", "AF", "KW_Merged", "AB_raw", "SO", "PY", "title", "note", "url", "DI"), drop = FALSE]
  names(df_out) <- c("Title", "Authors", "Keywords", "Abstract", "Journal", "PublicationYear", "title", "note", "url", "doi")
  return(df_out)
}

# PULIZIA bib_easy
bib_easy <- bib_easy %>%
  select(-PublicationType, -Language, -Database, -brain, -gut, -kidney, -distal_airways, -placenta)

# IMPORT DA RAYYAN ####
rayyan <- read_bibliography("rayyan_export_included.bib")

# JOIN FUZZY CON RAYYAN
bib_easy <- bib_easy %>% rownames_to_column("entry_id")

rayyan <- stringdist_left_join(
  rayyan,
  bib_easy[, c("Title", "entry_id")],
  by = c("title" = "Title"),
  method = "lv",
  max_dist = 10
)

bib_matched <- stringdist_inner_join(
  bib,
  rayyan[, c("entry_id", "title", "note")],
  by = c("TI_raw" = "title"),
  method = "lv",
  max_dist = 10
)

rayyan <- rayyan %>% column_to_rownames("entry_id")
bib_matched <- bib_matched %>% column_to_rownames("entry_id") %>% prepare_bib_table()

# ESTRAZIONE NOTE E LABELS
bib_matched <- bib_matched %>%
  mutate(
    Marta_decision = str_extract(note, 'Marta\\"=\\>\\\\"(Excluded|Included|Maybe)\\\\"') %>% str_extract('Excluded|Included|Maybe'),
    Giulia_decision = str_extract(note, 'Giulia\\"=\\>\\\\"(Excluded|Included|Maybe)\\\\"') %>% str_extract('Excluded|Included|Maybe'),
    labels = str_extract(note, 'RAYYAN-LABELS: .*?(\\||$|RAYYAN-EXCLUSION-REASONS:)') %>%
      str_remove_all('RAYYAN-LABELS: |\\|$') %>%
      str_replace_all('&amp;gt;', '>') %>%
      str_replace_all(",", ", "),
    exclusion_reason = str_extract(note, 'RAYYAN-EXCLUSION-REASONS: .*?(\\||$|USER-NOTES:)') %>%
      str_remove_all('RAYYAN-EXCLUSION-REASONS: |\\|$'),
    notes = str_extract(note, 'USER-NOTES: \\{.*?Marta\\"=\\>\\[.*?\\]') %>%
      str_extract('\\[.*?\\]') %>%
      str_remove_all('\\[|\\"|\\]')
  )

# ASSEGNAZIONE ORGANI ####
organs_list <- c("BBB", "gut", "kidney", "lung", "placenta", "1 organ")

for (organ in organs_list) {
  pattern <- paste0("\\b", organ, "\\b|", organ, "[-, ]\\w+")
  bib_matched[[organ]] <- ifelse(str_detect(bib_matched$labels, regex(pattern, ignore_case = TRUE)), "Y", "N")
}

bib_matched <- bib_matched %>% filter(if_any(all_of(organs_list), ~ . == "Y"))

# TABELLA RIASSUNTIVA ORGANI ####
organ_summary_df <- data.frame(
  Organ = organs_list,
  N_articles = sapply(organs_list, function(org) sum(bib_matched[[org]] == "Y"))
)

# PREPARAZIONE EXCEL ####
## all articles ####
bib_for_excel <- bib_matched %>%
  select(-title, -note, -Marta_decision, -Giulia_decision, -labels, -exclusion_reason, -notes) %>%
  rename(multiple_organs = `1 organ`, distal_airways = lung)

## table for each organ ####
BBB <- bib_for_excel %>%
  filter(BBB == "Y") %>%
  select(-BBB, -gut, -kidney, -distal_airways, -placenta, -multiple_organs)

gut <- bib_for_excel %>%
  filter(gut == "Y") %>%
  select(-BBB, -gut, -kidney, -distal_airways, -placenta, -multiple_organs)

kidney <- bib_for_excel %>%
  filter(kidney == "Y") %>%
  select(-BBB, -gut, -kidney, -distal_airways, -placenta, -multiple_organs)

lung <- bib_for_excel %>%
  filter(distal_airways == "Y") %>%
  select(-BBB, -gut, -kidney, -distal_airways, -placenta, -multiple_organs)

placenta <- bib_for_excel %>%
  filter(placenta == "Y") %>%
  select(-BBB, -gut, -kidney, -distal_airways, -placenta, -multiple_organs)

# AGGIORNA CONTEGGI
counts <- rbind(counts, data.frame(Table = "filtered_TitleAbstract", N_articles = nrow(bib_matched)))

# ESPORTAZIONE ####
save_table_to_excel(bib_for_excel, "filtered_TitleAbstract")

save_table_to_excel(BBB, "BBB")
save_table_to_excel(gut, "gut")
save_table_to_excel(kidney, "kidney")
save_table_to_excel(lung, "distal_airways")
save_table_to_excel(placenta, "placenta")

save_table_to_excel(organ_summary_df, "Organ_Summary")

save_table_to_excel(counts, "Counts")

# SALVARE .BIB PER CIASCUN ORGANO ####

# LISTA ORGANI SENZA "1 organ"
organs_list <- c("BBB", "gut", "kidney", "lung", "placenta")

# FUNZIONE PER CREARE FILE .bib PER UN ORGAN
format_authors <- function(authors_raw) {
  if (is.na(authors_raw) || authors_raw == "") return(NA)
  authors <- unlist(strsplit(authors_raw, ";\\s*| and |,\\s*"))
  authors <- authors[authors != ""]
  return(authors)
}

save_bib_for_organ <- function(organ_name, bib_data) {
  bib_minimal <- bib_data %>%
    filter(.data[[organ_name]] == "Y") %>%
    select(Authors, Title, Journal, PublicationYear, Abstract, Keywords) %>%
    rename(
      author = Authors,
      title = Title,
      journal = Journal,
      year = PublicationYear,
      abstract = Abstract,
      keywords = Keywords
    )
  
  bib_entries_list <- lapply(1:nrow(bib_minimal), function(i) {
    row <- bib_minimal[i, ]
    authors <- format_authors(row$author)
    if (is.na(row$title) || is.na(row$journal) || is.na(row$year)) return(NULL)
    
    bibentry(
      bibtype = "Article",
      key = paste0(organ_name, "_ref", i),
      author = authors,
      title = row$title,
      journal = row$journal,
      year = as.character(row$year),
      other = list(
        abstract = row$abstract,
        keywords = row$keywords
      )
    )
  })
  
  bib_entries_list <- Filter(Negate(is.null), bib_entries_list)
  bib_entries <- as.BibEntry(do.call("c", bib_entries_list))
  
  WriteBib(bib_entries, file = paste0(organ_name, "_for_rayyan_import.bib"))
}

# SALVA FILE .bib PER OGNI ORGANO ===
for (organ in organs_list) {
  save_bib_for_organ(organ, bib_matched)
}

save.image(file = ".RData")
