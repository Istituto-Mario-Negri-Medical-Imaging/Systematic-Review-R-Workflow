# IMPORT RECORD FROM PUBMED AND SCOPUS ####
## PULIZIA E LIBRERIE ####
rm(list = ls())
source("Libraries.R")

## FUNZIONI UTILI ####
save_table_to_excel <- function(data, sheet_name, file_name = "Output_01.xlsx") {
  wb <- if (file.exists(file_name)) loadWorkbook(file_name) else createWorkbook()
  if (!(sheet_name %in% names(wb))) addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, x = data)
  saveWorkbook(wb, file_name, overwrite = TRUE)
}

write_counts_to_excel <- function(counts_df, sheet_name = "Counts", file_name = "Output_01.xlsx") {
  wb <- if (file.exists(file_name)) loadWorkbook(file_name) else createWorkbook()
  if (!(sheet_name %in% names(wb))) addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, x = counts_df)
  saveWorkbook(wb, file_name, overwrite = TRUE)
}

prepare_bib_table <- function(df) {
  cols <- c("TI_raw", "AF", "KW_Merged", "DT", "AB_raw", "SO", "LA", "PY")
  if ("DB_Original" %in% names(df)) {
    cols <- c(cols, "DB_Original")
  } else if ("DB" %in% names(df)) {
    cols <- c(cols, "DB")
  }
  df_out <- df[, cols, drop = FALSE]
  names(df_out) <- c("Title", "Autors", "Keywords", "PublicationType", "Abstract", "Journal", "Language", "PublicationYear", "Database")
  return(df_out)
}

format_authors <- function(authors_raw) {
  if (is.na(authors_raw) || authors_raw == "") return(NA)
  authors <- unlist(strsplit(authors_raw, ";\\s*| and |,\\s*"))
  authors <- authors[authors != ""]
  return(authors)
}

# IMPORT DATI ####
pubmed_df <- convert2df("datasets_20250721/pubmed-Lab-On-A-C-set.nbib", dbsource = "pubmed", format = "pubmed")
scopus_files <- list.files("datasets_20250721", pattern = "scopus_.*\\.bib$", full.names = TRUE)
scopus_df <- convert2df(scopus_files, dbsource = "scopus", format = "bibtex")

save_table_to_excel(prepare_bib_table(pubmed_df), "Pubmed")
save_table_to_excel(prepare_bib_table(scopus_df), "Scopus")

# CONTEGGI INIZIALI
counts <- data.frame(Table = c("pubmed_df", "scopus_df"),
                     N_articles = c(nrow(pubmed_df), nrow(scopus_df)))

# UNIONE DATABASE ####
bib <- mergeDbSources(pubmed_df, scopus_df, remove.duplicated = TRUE)
counts <- rbind(counts, data.frame(Table = "MergeDB_noDuplicates", N_articles = nrow(bib)))
save_table_to_excel(prepare_bib_table(bib), "MergeDB")

# FILTRI ####
## FILTRO: TIPO PUBBLICAZIONE ####
scopus_n_types <- read_excel("List_of_publication_types.xlsx", sheet = "Scopus") %>%
  filter(Y_N == "N") %>% pull(Publication_Type)
pubmed_n_types <- read_excel("List_of_publication_types.xlsx", sheet = "Pubmed") %>%
  filter(Y_N == "N") %>% pull(Publication_Type)

bib <- bib %>%
  mutate(pub_type_passed = case_when(
    DB_Original == "SCOPUS" & str_detect(tolower(DT), paste(tolower(scopus_n_types), collapse = "|")) ~ "N",
    DB_Original == "PUBMED" & str_detect(tolower(DT), paste(tolower(pubmed_n_types), collapse = "|")) ~ "N",
    is.na(DT) ~ "Y",
    TRUE ~ "Y"
  )) %>%
  filter(pub_type_passed != "N") %>%
  select(-pub_type_passed)

counts <- rbind(counts, data.frame(Table = "filtered_JournalArticle", N_articles = nrow(bib)))
save_table_to_excel(prepare_bib_table(bib), "filtered_PubType")

## FILTRO: LINGUA ####
bib <- bib %>%
  mutate(language_passed = ifelse(LA %in% c("ENGLISH", "ENGLISH ENGLISH", "ENG"), "Y", "N")) %>%
  filter(language_passed != "N") %>%
  select(-language_passed)

counts <- rbind(counts, data.frame(Table = "filtered_English", N_articles = nrow(bib)))
save_table_to_excel(prepare_bib_table(bib), "filtered_Eng")

## FILTRO: ANNO PUBBLICAZIONE ####
bib <- bib %>%
  mutate(year_passed = ifelse(as.numeric(PY) >= 2014, "Y", "N")) %>%
  filter(year_passed != "N") %>%
  select(-year_passed)

counts <- rbind(counts, data.frame(Table = "filtered_Year", N_articles = nrow(bib)))
save_table_to_excel(prepare_bib_table(bib), "filtered_Year")


## FILTRO: ORGANI ####
organ_terms <- read_excel("Organ_terms_list.xlsx")
organs_list <- lapply(organ_terms, function(col) {
  terms <- na.omit(col)
  terms[terms != colnames(organ_terms)[which(sapply(organ_terms, identical, col))]]
}) %>% 
  setNames(tolower(names(organ_terms))) %>%
  lapply(tolower)

bib <- bib %>%
  mutate(full_text = paste(TI_raw, AB_raw, KW_Merged, sep = " "),
         full_text = tolower(full_text))

for (organ in names(organs_list)) {
  pattern <- paste0("\\b(", paste(organs_list[[organ]], collapse = "|"), ")\\b")
  bib[[organ]] <- ifelse(str_detect(bib$full_text, pattern), "Y", "N")
}

organ_cols <- names(organs_list)
bib <- bib %>% filter(if_any(all_of(organ_cols), ~ . == "Y"))

counts <- rbind(counts, data.frame(Table = "filtered_Organs", N_articles = nrow(bib)))

bib_easy <- prepare_bib_table(bib)
bib_easy <- cbind(bib_easy, bib[, organ_cols])
save_table_to_excel(bib_easy, "filtered_Organs")
save_table_to_excel(counts, "Counts")

# TABELLA RIASSUNTIVA ORGANI ####
organ_summary <- sapply(organ_cols, function(col) sum(bib[[col]] == "Y"))
organ_summary_df <- data.frame(Organ = names(organ_summary), N_articles = as.integer(organ_summary))
save_table_to_excel(organ_summary_df, "Organ_Summary")

# EXPORT BIBLIOGRAFIA FILTRATA ####
## .RData ####
save(bib, file = "filtered_bibliography.RData")

## .bib ####
# Prepara i dati
bib_minimal <- bib %>%
  select(AU, TI_raw, SO, PY, AB_raw, KW_Merged) %>%
  rename(
    author = AU,
    title = TI_raw,
    journal = SO,
    year = PY,
    abstract = AB_raw,
    keywords = KW_Merged
  )

# Crea lista di voci bibentry
bib_entries_list <- lapply(1:nrow(bib_minimal), function(i) {
  row <- bib_minimal[i, ]
  authors <- format_authors(row$author)
  if (is.na(row$title) || is.na(row$journal) || is.na(row$year)) return(NULL)
  
  bibentry(
    bibtype = "Article",
    key = paste0("ref", i),
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

# Rimuovi eventuali NULL
bib_entries_list <- Filter(Negate(is.null), bib_entries_list)

# Converte in oggetto BibEntry
bib_entries <- as.BibEntry(do.call("c", bib_entries_list))

# Scrive il file .bib compatibile con Rayyan
WriteBib(bib_entries, file = "filtered_bibliography_rayyan_import.bib")

rm(bib_entries)
rm(bib_entries_list)
rm(bib_minimal)
rm(organ_summary_df)
rm(organ_terms)
rm(organs_list)
rm(pubmed_df)
rm(scopus_df)

save.image(file = ".RData")
