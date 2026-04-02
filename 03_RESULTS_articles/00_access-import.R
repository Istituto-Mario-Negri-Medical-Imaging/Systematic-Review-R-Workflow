# 0_Access_import Tables ####

# PULIZIA E LIBRERIE ####
setwd("../03_RESULTS_articles")
load("../02_RAYYAN/.RData")
rm(list = setdiff(ls(), c("bib_for_excel")))
source("../01_IMPORT/Libraries.R")

# Connect to Access Database ####
db_path <- "../Database.accdb"

con <- dbConnect(odbc::odbc(),
                 .connection_string = paste0(
                   "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
                   "DBQ=", db_path, ";")
)

# Read tables from database ####
access_object_list <- data.frame(dbListTables(con))

Tbl_Articles <- dbReadTable(con, "Tbl_Articles")
Tbl_Cells <- dbReadTable(con, "Tbl_Cells")
Tbl_Membranes <- dbReadTable(con, "Tbl_Membranes")
Tbl_Readouts <- dbReadTable(con, "Tbl_Readouts")

join_Articles_Cells <- dbReadTable(con, "join_Articles_Cells")
join_Articles_Membranes <- dbReadTable(con, "join_Articles_Membranes")
join_Articles_Readouts <- dbReadTable(con, "join_Articles_Readouts")

# Mappa organi → nome colonna in Tbl_Articles
col_map <- list(
  Brain         = "IncExc_Brain",
  Gut            = "IncExc_Gut",
  Kidney         = "IncExc_Kidney",
  Lung           = "IncExc_Lung",
  Placenta       = "IncExc_Placenta"
)

# Tabelle Included / Excluded ####

for (org in names(col_map)) {
  status_col <- col_map[[org]]
  
  # Controllo che la colonna esista
  if (!status_col %in% names(Tbl_Articles)) {
    warning(paste("La colonna", status_col, "non esiste in Tbl_Articles."))
    next
  }
  
  # Tabelle Included
  included_df <- Tbl_Articles %>%
    filter(.data[[status_col]] == "Included")
  
  # Tabelle Excluded
  excluded_df <- Tbl_Articles %>%
    filter(.data[[status_col]] == "Excluded")
  
  # Nomi degli oggetti da creare
  included_name <- paste0("Article_Included_", org)
  excluded_name <- paste0("Article_Excluded_", org)
  
  # Assegna in ambiente globale
  assign(included_name, included_df, envir = .GlobalEnv)
  assign(excluded_name, excluded_df, envir = .GlobalEnv)
}

Article_Included_all <- bind_rows(
  Article_Included_Gut,
  Article_Included_Kidney,
  Article_Included_Lung,
  Article_Included_Brain,
  Article_Included_Placenta
)

Article_Excluded_all <- bind_rows(
  Article_Excluded_Gut,
  Article_Excluded_Kidney,
  Article_Excluded_Lung,
  Article_Excluded_Brain,
  Article_Excluded_Placenta
)

# Tabelle join membrane ####
create_membrane_table <- function(article_tbl, organ_name) {
  article_tbl %>%
    inner_join(
      join_Articles_Membranes %>%
        filter(Organ_membrane == organ_name),
      by = "ID_Article"
    ) %>%
    inner_join(
      Tbl_Membranes,
      by = "ID_MembraneMaterial"
    )
}

Membranes_Gut      <- create_membrane_table(Article_Included_Gut, "Gut")
Membranes_Kidney   <- create_membrane_table(Article_Included_Kidney, "Kidney")
Membranes_Lung     <- create_membrane_table(Article_Included_Lung, "Lung")
Membranes_Brain    <- create_membrane_table(Article_Included_Brain, "Brain")
Membranes_Placenta <- create_membrane_table(Article_Included_Placenta, "Placenta")

Membranes_All <- bind_rows(
  Membranes_Gut,
  Membranes_Kidney,
  Membranes_Lung,
  Membranes_Brain,
  Membranes_Placenta
)

# Tabelle join cells ####
create_cell_table <- function(article_tbl, organ_name) {
  article_tbl %>%
    inner_join(
      join_Articles_Cells %>%
        filter(Organ_cell == organ_name),
      by = "ID_Article"
    ) %>%
    inner_join(
      Tbl_Cells,
      by = "ID_Cell"
    )
}

Cell_Gut      <- create_cell_table(Article_Included_Gut, "Gut")
Cell_Kidney   <- create_cell_table(Article_Included_Kidney, "Kidney")
Cell_Lung     <- create_cell_table(Article_Included_Lung, "Lung")
Cell_Brain    <- create_cell_table(Article_Included_Brain, "Brain")
Cell_Placenta <- create_cell_table(Article_Included_Placenta, "Placenta")

Cell_All <- bind_rows(
  Cell_Gut,
  Cell_Kidney,
  Cell_Lung,
  Cell_Brain,
  Cell_Placenta
)

# Tabelle join readouts ####
create_readout_table <- function(article_tbl, organ_name) {
  article_tbl %>%
    inner_join(
      join_Articles_Readouts %>%
        filter(Organ_readout == organ_name),
      by = "ID_Article"
    ) %>%
    inner_join(
      Tbl_Readouts,
      by = "ID_Readout"
    )
}

Readout_Gut      <- create_readout_table(Article_Included_Gut, "Gut")
Readout_Kidney   <- create_readout_table(Article_Included_Kidney, "Kidney")
Readout_Lung     <- create_readout_table(Article_Included_Lung, "Lung")
Readout_Brain    <- create_readout_table(Article_Included_Brain, "Brain")
Readout_Placenta <- create_readout_table(Article_Included_Placenta, "Placenta")

Readout_All <- bind_rows(
  Readout_Gut,
  Readout_Kidney,
  Readout_Lung,
  Readout_Brain,
  Readout_Placenta
)


# Definisci i colori, organi, liste #### 
colori_organi <- c( 
  Gut = "#EADCF4", 
  Kidney = "#FFF2CC", 
  Lung = "#DEEAF6", 
  Brain = "#E2EFD9", 
  Placenta = "#FBE4D5", 
  Overall = "#F2F2F2" ) 

organi <- c("Gut", "Kidney", "Lung", "Brain", "Placenta") 

order <- c("Gut", "Kidney", "Lung", "Brain", "Placenta", "Overall")

article_list <- list(
  Gut      = Article_Included_Gut,
  Kidney   = Article_Included_Kidney,
  Lung     = Article_Included_Lung,
  Brain    = Article_Included_Brain,
  Placenta = Article_Included_Placenta,
  All      = Article_Included_all
)

membrane_list <- list(
  Gut      = Membranes_Gut,
  Kidney   = Membranes_Kidney,
  Lung     = Membranes_Lung,
  Brain    = Membranes_Brain,
  Placenta = Membranes_Placenta,
  All      = Membranes_All
)

cell_list <- list(
  Gut      = Cell_Gut,
  Kidney   = Cell_Kidney,
  Lung     = Cell_Lung,
  Brain    = Cell_Brain,
  Placenta = Cell_Placenta,
  All      = Cell_All
)

readout_list <- list(
  Gut      = Readout_Gut,
  Kidney   = Readout_Kidney,
  Lung     = Readout_Lung,
  Brain    = Readout_Brain,
  Placenta = Readout_Placenta,
  All      = Readout_All
)

# Export Excel ####
#Article Included
write_xlsx(
  list(
    Sheet1 = Article_Included_all
  ),
  "Articles_Included_All_Table.xlsx"
)

# Article Excluded
write_xlsx(
  list(
    Sheet1 = Article_Excluded_all
  ),
  "Articles_Excluded_All_Table.xlsx"
)

# Membrane
write_xlsx(
  list(
    Sheet1 = Membranes_All
  ),
  "Membranes_All_Table.xlsx"
)

# Cell
write_xlsx(
  list(
    Sheet1 = Cell_All
  ),
  "Cells_All_Table.xlsx"
)

# Readout
write_xlsx(
  list(
    Sheet1 = Readout_All
  ),
  "Readouts_All_Table.xlsx"
)

## Opzionale: riepilogo rapido ####
cat("\nRiepilogo articoli per organo:\n")
for (org in names(col_map)) {
  inc <- paste0("Article_Included_", org)
  exc <- paste0("Article_Excluded_", org)
  if (exists(inc) && exists(exc)) {
    cat(sprintf("  %s → Included: %d | Excluded: %d\n",
                org,
                nrow(get(inc)),
                nrow(get(exc))))
  }
}
